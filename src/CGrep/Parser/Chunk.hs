{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
--
-- Copyright (c) 2013-2025 Nicola Bonelli <nicola@larthia.com>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module CGrep.Parser.Chunk (
    parseChunks,
    parseChunksRef,
    Chunk (..),
    cOffset,
    MatchLine (..),
    ChunkType,
    pattern ChunkIdentifier,
    pattern ChunkKeyword,
    pattern ChunkDigit,
    pattern ChunkBracket,
    pattern ChunkString,
    pattern ChunkNativeType,
    pattern ChunkOperator,
    pattern ChunkUnspec,
) where

import CGrep.FileTypeMap (CharSet (..), FileTypeInfo (..), IsCharSet (..))
import CGrep.Parser.Char (isBracket', isCharNumber, isDigit, isSpace)
import CGrep.Text (iterM, textOffsetWord8)
import Control.Monad.ST (ST, runST)
import Data.Foldable (toList)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Sequence ((|>))
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import qualified Data.Text.Unsafe as TU
import Data.Word (Word8)
import GHC.Exts (Char (C#), Int (I#), chr#, inline)

unsafeChr8 :: Word8 -> Char
unsafeChr8 !w =
    let !(I# i#) = fromIntegral w
     in C# (chr# i#)
{-# INLINE unsafeChr8 #-}

newtype ChunkType = ChunkType {_unChunkType :: Word8}
    deriving newtype (Eq, Ord)

instance Show ChunkType where
    show ChunkUnspec = "*"
    show ChunkIdentifier = "identifier"
    show ChunkKeyword = "keyword"
    show ChunkDigit = "digit"
    show ChunkBracket = "bracket"
    show ChunkOperator = "operator"
    show ChunkString = "string"
    show ChunkNativeType = "native-type"
    {-# INLINE show #-}

pattern ChunkUnspec :: ChunkType
pattern ChunkUnspec = ChunkType 0

pattern ChunkIdentifier :: ChunkType
pattern ChunkIdentifier = ChunkType 1

pattern ChunkKeyword :: ChunkType
pattern ChunkKeyword = ChunkType 2

pattern ChunkDigit :: ChunkType
pattern ChunkDigit = ChunkType 3

pattern ChunkBracket :: ChunkType
pattern ChunkBracket = ChunkType 4

pattern ChunkOperator :: ChunkType
pattern ChunkOperator = ChunkType 5

pattern ChunkString :: ChunkType
pattern ChunkString = ChunkType 6

pattern ChunkNativeType :: ChunkType
pattern ChunkNativeType = ChunkType 7

{-# COMPLETE ChunkIdentifier, ChunkKeyword, ChunkDigit, ChunkBracket, ChunkOperator, ChunkString, ChunkNativeType, ChunkUnspec #-}

data Chunk = Chunk
    { cTyp :: {-# UNPACK #-} !ChunkType
    , cToken :: {-# UNPACK #-} !T.Text
    }
    deriving stock (Eq, Show, Ord)

cOffset :: Chunk -> Int
cOffset (Chunk _ t) = textOffsetWord8 t
{-# INLINE cOffset #-}

data MatchLine = MatchLine
    { mlOffset :: {-# UNPACK #-} !Int
    , mlChunks :: [Chunk]
    }
    deriving stock (Eq, Show)

{-# INLINE parseChunks #-}
parseChunks :: Maybe FileTypeInfo -> T.Text -> [Chunk]
parseChunks l txt = case l >>= \FileTypeInfo{..} -> ftIdentCharSet of
    Just (None, None) -> parseChunksFast @'None @'None txt
    Just (Alpha_, AlphaNum_') -> parseChunksFast @'Alpha_ @'AlphaNum_' txt
    Just (Alpha_, AlphaNum_) -> parseChunksFast @'Alpha_ @'AlphaNum_ txt
    Just (Alpha, AlphaNum_) -> parseChunksFast @'Alpha @'AlphaNum_ txt
    Just (Alpha_, AlphaNumDash_) -> parseChunksFast @'Alpha_ @'AlphaNumDash_ txt
    Just (AlphaDash_, AlphaNumDash_) -> parseChunksFast @'AlphaDash_ @'AlphaNumDash_ txt
    Just (ClojureIdentStart, ClojureIdentCont) -> parseChunksFast @'ClojureIdentStart @'ClojureIdentCont txt
    Just (CSharpIdentStart, CSharpIdentCont) -> parseChunksFast @'CSharpIdentStart @'CSharpIdentCont txt
    Just (HtmlIdentStart, HtmlIdentCont) -> parseChunksFast @'HtmlIdentStart @'HtmlIdentCont txt
    Just (JavaIdentStart, JavaIdentCont) -> parseChunksFast @'JavaIdentStart @'JavaIdentCont txt
    Just (JuliaIdentStart, JuliaIdentCont) -> parseChunksFast @'JuliaIdentStart @'JuliaIdentCont txt
    Just (ListIdent, ListIdent) -> parseChunksFast @'ListIdent @'ListIdent txt
    Just (Unicode_, UnicodeNum_) -> parseChunksFast @'Unicode_ @'UnicodeNum_ txt
    Just (UnicodeDollar_, UnicodeNumDollar_) -> parseChunksFast @'UnicodeDollar_ @'UnicodeNumDollar_ txt
    Just (Unicode_, UnicodeNum_') -> parseChunksFast @'Unicode_ @'UnicodeNum_' txt
    Just (Unicode_, UnicodeNumDollar_) -> parseChunksFast @'Unicode_ @'UnicodeNumDollar_ txt
    Just (UnicodeXIDStart_, UnicodeNumXIDCont_) -> parseChunksFast @'UnicodeXIDStart_ @'UnicodeNumXIDCont_ txt
    Just (AgdaIdent, AgdaIdent) -> parseChunksFast @'AgdaIdent @'AgdaIdent txt
    Nothing -> parseChunksFast @'Unicode_ @'UnicodeNum_ txt
    charsets -> error $ "CGrep: unsupported CharSet combination: " <> show charsets

parseChunksFast :: forall (cs1 :: CharSet) (csN :: CharSet). (IsCharSet cs1, IsCharSet csN) => T.Text -> [Chunk]
parseChunksFast txt@(TI.Text arr srcOff byteLen)
    | byteLen <= 0 = []
    | otherwise = goSpace srcOff []
  where
    !endOff = srcOff + byteLen

    {-# INLINE decodeAt #-}
    decodeAt :: Int -> (# Char, Int #)
    decodeAt !ix =
        let !b0 = A.unsafeIndex arr ix
         in if b0 < 0x80
                then (# unsafeChr8 b0, 1 #)
                else
                    let TU.Iter !c !d = TU.iter txt (ix - srcOff)
                     in (# c, d #)

    {-# INLINE mkUnspec #-}
    mkUnspec :: Int -> Int -> Chunk
    mkUnspec !beg !end = Chunk ChunkUnspec (TI.Text arr beg (end - beg))

    goSpace :: Int -> [Chunk] -> [Chunk]
    goSpace !ix !acc
        | ix >= endOff = reverse acc
        | otherwise =
            let !b0 = A.unsafeIndex arr ix
             in if b0 == 0x20
                    then goSpace (ix + 1) acc
                    else
                        let !(# !x, !delta #) =
                                if b0 < 0x80
                                    then (# unsafeChr8 b0, 1 #)
                                    else
                                        let TU.Iter !c !d = TU.iter txt (ix - srcOff)
                                         in (# c, d #)
                            !nextIx = ix + delta
                         in if
                                | isSpace x -> goSpace nextIx acc
                                | inline (isValidChar @cs1 x) -> goAlpha ix nextIx acc
                                | isDigit x -> goDigit ix nextIx acc
                                | isBracket' x ->
                                    let !c = mkUnspec ix nextIx
                                     in goSpace nextIx (c : acc)
                                | otherwise -> goOther ix nextIx acc

    goAlpha :: Int -> Int -> [Chunk] -> [Chunk]
    goAlpha !tokStart !ix !acc
        | ix >= endOff =
            let !c = mkUnspec tokStart endOff
             in reverse (c : acc)
        | otherwise =
            let !(# !x, !delta #) = decodeAt ix
                !nextIx = ix + delta
             in if inline (isValidChar @csN x)
                    then goAlpha tokStart nextIx acc
                    else
                        let !c1 = mkUnspec tokStart ix
                            !acc1 = c1 : acc
                         in if
                                | isSpace x -> goSpace nextIx acc1
                                | isBracket' x ->
                                    let !c2 = mkUnspec ix nextIx
                                     in goSpace nextIx (c2 : acc1)
                                | otherwise -> goOther ix nextIx acc1

    goDigit :: Int -> Int -> [Chunk] -> [Chunk]
    goDigit !tokStart !ix !acc
        | ix >= endOff =
            let !c = mkUnspec tokStart endOff
             in reverse (c : acc)
        | otherwise =
            let !(# !x, !delta #) = decodeAt ix
                !nextIx = ix + delta
             in if isCharNumber x
                    then goDigit tokStart nextIx acc
                    else
                        let !c1 = mkUnspec tokStart ix
                            !acc1 = c1 : acc
                         in if
                                | isSpace x -> goSpace nextIx acc1
                                | inline (isValidChar @cs1 x) -> goAlpha ix nextIx acc1
                                | isBracket' x ->
                                    let !c2 = mkUnspec ix nextIx
                                     in goSpace nextIx (c2 : acc1)
                                | otherwise -> goOther ix nextIx acc1

    goOther :: Int -> Int -> [Chunk] -> [Chunk]
    goOther !tokStart !ix !acc
        | ix >= endOff =
            let !c = mkUnspec tokStart endOff
             in reverse (c : acc)
        | otherwise =
            let !(# !x, !delta #) = decodeAt ix
                !nextIx = ix + delta
             in if
                    | isSpace x ->
                        let !c1 = mkUnspec tokStart ix
                         in goSpace nextIx (c1 : acc)
                    | inline (isValidChar @cs1 x) ->
                        let !c1 = mkUnspec tokStart ix
                         in goAlpha ix nextIx (c1 : acc)
                    | isDigit x ->
                        if ix - tokStart == 1 && A.unsafeIndex arr tokStart == 0x2E
                            then goDigit tokStart nextIx acc
                            else
                                let !c1 = mkUnspec tokStart ix
                                 in goDigit ix nextIx (c1 : acc)
                    | isBracket' x ->
                        let !c1 = mkUnspec tokStart ix
                            !c2 = mkUnspec ix nextIx
                         in goSpace nextIx (c2 : c1 : acc)
                    | otherwise -> goOther tokStart nextIx acc

-- ---------------------------------------------------------------------------
-- Reference STRef + Seq implementation (retained for QuickCheck & corpus
-- differential testing).
-- ---------------------------------------------------------------------------

newtype ChunkState = ChunkState {_unChunkState :: Word8}
    deriving newtype (Eq, Ord)

pattern StateSpace :: ChunkState
pattern StateSpace = ChunkState 0

pattern StateAlpha :: ChunkState
pattern StateAlpha = ChunkState 1

pattern StateDigit :: ChunkState
pattern StateDigit = ChunkState 2

pattern StateBracket :: ChunkState
pattern StateBracket = ChunkState 3

pattern StateOther :: ChunkState
pattern StateOther = ChunkState 4

{-# COMPLETE StateSpace, StateAlpha, StateDigit, StateBracket, StateOther #-}

(<~) :: STRef s a -> a -> ST s ()
ref <~ !x = writeSTRef ref x
{-# INLINE (<~) #-}

toChunk :: T.Text -> Maybe Int -> Int -> Chunk
toChunk _ Nothing _ = error "CGrep.Parser.Chunk.toChunk: Nothing (Internal Error)"
toChunk text (Just beg) cur =
    let chunk = TU.takeWord8 (cur - beg) (TU.dropWord8 beg text)
     in Chunk ChunkUnspec chunk
{-# INLINE toChunk #-}

parseChunksRef :: Maybe FileTypeInfo -> T.Text -> [Chunk]
parseChunksRef l txt = toList $ runST $ case l >>= \FileTypeInfo{..} -> ftIdentCharSet of
    Just (None, None) -> parseChunksRef' @'None @'None txt
    Just (Alpha_, AlphaNum_') -> parseChunksRef' @'Alpha_ @'AlphaNum_' txt
    Just (Alpha_, AlphaNum_) -> parseChunksRef' @'Alpha_ @'AlphaNum_ txt
    Just (Alpha, AlphaNum_) -> parseChunksRef' @'Alpha @'AlphaNum_ txt
    Just (Alpha_, AlphaNumDash_) -> parseChunksRef' @'Alpha_ @'AlphaNumDash_ txt
    Just (AlphaDash_, AlphaNumDash_) -> parseChunksRef' @'AlphaDash_ @'AlphaNumDash_ txt
    Just (ClojureIdentStart, ClojureIdentCont) -> parseChunksRef' @'ClojureIdentStart @'ClojureIdentCont txt
    Just (CSharpIdentStart, CSharpIdentCont) -> parseChunksRef' @'CSharpIdentStart @'CSharpIdentCont txt
    Just (HtmlIdentStart, HtmlIdentCont) -> parseChunksRef' @'HtmlIdentStart @'HtmlIdentCont txt
    Just (JavaIdentStart, JavaIdentCont) -> parseChunksRef' @'JavaIdentStart @'JavaIdentCont txt
    Just (JuliaIdentStart, JuliaIdentCont) -> parseChunksRef' @'JuliaIdentStart @'JuliaIdentCont txt
    Just (ListIdent, ListIdent) -> parseChunksRef' @'ListIdent @'ListIdent txt
    Just (Unicode_, UnicodeNum_) -> parseChunksRef' @'Unicode_ @'UnicodeNum_ txt
    Just (UnicodeDollar_, UnicodeNumDollar_) -> parseChunksRef' @'UnicodeDollar_ @'UnicodeNumDollar_ txt
    Just (Unicode_, UnicodeNum_') -> parseChunksRef' @'Unicode_ @'UnicodeNum_' txt
    Just (Unicode_, UnicodeNumDollar_) -> parseChunksRef' @'Unicode_ @'UnicodeNumDollar_ txt
    Just (UnicodeXIDStart_, UnicodeNumXIDCont_) -> parseChunksRef' @'UnicodeXIDStart_ @'UnicodeNumXIDCont_ txt
    Just (AgdaIdent, AgdaIdent) -> parseChunksRef' @'AgdaIdent @'AgdaIdent txt
    Nothing -> parseChunksRef' @'Unicode_ @'UnicodeNum_ txt
    charsets -> error $ "CGrep: unsupported CharSet combination: " <> show charsets

parseChunksRef' :: forall (cs1 :: CharSet) (csN :: CharSet) s. (IsCharSet cs1, IsCharSet csN) => T.Text -> ST s (S.Seq Chunk)
parseChunksRef' txt = do
    stateR <- newSTRef StateSpace
    accR <- newSTRef Nothing
    tokensR <- newSTRef S.empty
    iterM txt $ \(# x, offset, _delta #) -> do
        state <- readSTRef stateR
        acc <- readSTRef accR
        tokens <- readSTRef tokensR
        case state of
            StateSpace ->
                if
                    | isSpace x -> do stateR <~ StateSpace; accR <~ Nothing
                    | isValidChar @cs1 x -> do stateR <~ StateAlpha; accR <~ Just offset
                    | isDigit x -> do stateR <~ StateDigit; accR <~ Just offset
                    | isBracket' x -> do stateR <~ StateBracket; accR <~ Just offset
                    | otherwise -> do stateR <~ StateOther; accR <~ Just offset
            StateAlpha ->
                if
                    | isValidChar @csN x -> do stateR <~ StateAlpha
                    | isSpace x -> do stateR <~ StateSpace; accR <~ Nothing; tokensR <~ (tokens |> toChunk txt acc offset)
                    | isBracket' x -> do stateR <~ StateBracket; accR <~ Just offset; tokensR <~ (tokens |> toChunk txt acc offset)
                    | otherwise -> do stateR <~ StateOther; accR <~ Just offset; tokensR <~ (tokens |> toChunk txt acc offset)
            StateDigit ->
                if
                    | isCharNumber x -> do stateR <~ StateDigit
                    | isSpace x -> do stateR <~ StateSpace; accR <~ Nothing; tokensR <~ (tokens |> toChunk txt acc offset)
                    | isValidChar @cs1 x -> do stateR <~ StateAlpha; accR <~ Just offset; tokensR <~ (tokens |> toChunk txt acc offset)
                    | isBracket' x -> do stateR <~ StateBracket; accR <~ Just offset; tokensR <~ (tokens |> toChunk txt acc offset)
                    | otherwise -> do stateR <~ StateOther; accR <~ Just offset; tokensR <~ (tokens |> toChunk txt acc offset)
            StateBracket ->
                if
                    | isSpace x -> do stateR <~ StateSpace; accR <~ Nothing; tokensR <~ (tokens |> toChunk txt acc offset)
                    | isValidChar @cs1 x -> do stateR <~ StateAlpha; accR <~ Just offset; tokensR <~ (tokens |> toChunk txt acc offset)
                    | isDigit x -> do stateR <~ StateDigit; accR <~ Just offset; tokensR <~ (tokens |> toChunk txt acc offset)
                    | isBracket' x -> do stateR <~ StateBracket; accR <~ Just offset; tokensR <~ (tokens |> toChunk txt acc offset)
                    | otherwise -> do stateR <~ StateOther; accR <~ Just offset; tokensR <~ (tokens |> toChunk txt acc offset)
            StateOther ->
                if
                    | isSpace x -> do stateR <~ StateSpace; accR <~ Nothing; tokensR <~ (tokens |> toChunk txt acc offset)
                    | isValidChar @cs1 x -> do stateR <~ StateAlpha; accR <~ Just offset; tokensR <~ (tokens |> toChunk txt acc offset)
                    | isDigit x ->
                        if acc == Just (offset - 1)
                            && TU.unsafeHead (TU.dropWord8 (offset - 1) txt) == '.'
                            then do stateR <~ StateDigit
                            else do stateR <~ StateDigit; accR <~ Just offset; tokensR <~ (tokens |> toChunk txt acc offset)
                    | isBracket' x -> do stateR <~ StateBracket; accR <~ Just offset; tokensR <~ (tokens |> toChunk txt acc offset)
                    | otherwise -> do stateR <~ StateOther

    tokens <- readSTRef tokensR
    lastAcc <- readSTRef accR

    case lastAcc of
        Nothing -> return tokens
        Just off
            | off == TU.lengthWord8 txt -> return tokens
            | otherwise -> return $ tokens |> toChunk txt lastAcc (TU.lengthWord8 txt)

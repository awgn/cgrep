--
-- Copyright (c) 2013-2023 Nicola Bonelli <nicola@larthia.com>
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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module CGrep.Parser.Chunk (
    parseChunks,
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

import CGrep.FileTypeMap (FileTypeInfo (..), CharSet (..), IsCharSet (..))

import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

-- import Data.Maybe (fromMaybe, fromJust)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (|>))
import qualified Data.Sequence as S
import Data.Word (Word8)
import qualified Data.Text as T
-- import qualified Data.Text.Internal.StrictBuilder as T
import qualified Data.Text.Unsafe as TU

import CGrep.Text (iterM, textOffsetWord8)
import Data.Coerce (coerce)
import CGrep.Parser.Char (isDigit, isSpace, isCharNumber, isBracket')

newtype ChunkType = ChunkType {unChunkType :: Word8}
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

newtype ChunkState = ChunkState {unChunkState :: Word8}
    deriving newtype (Eq, Ord)

instance Show ChunkState where
    show StateSpace = "space"
    show StateAlpha = "alpha"
    show StateDigit = "digit"
    show StateBracket = "bracket"
    show StateOther = "other"
    {-# INLINE show #-}

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

(<~) :: STRef s a -> a -> ST s ()
ref <~ !x = writeSTRef ref x
{-# INLINE (<~) #-}


toChunk :: T.Text ->  Maybe Int -> Int -> Chunk
toChunk _ Nothing _ = error "CGrep.Parser.Chunk.toChunk: Nothing (Internal Error)"
toChunk text (Just beg) cur =
    let chunk = TU.takeWord8 (cur - beg) (TU.dropWord8 beg text)
       in Chunk ChunkUnspec chunk
{-# INLINE toChunk #-}


{-# COMPLETE StateSpace, StateAlpha, StateDigit, StateBracket, StateOther #-}

{-# INLINE parseChunks #-}
parseChunks :: Maybe FileTypeInfo -> T.Text -> S.Seq Chunk
parseChunks l txt = runST $ case l >>= \FileTypeInfo{..} -> ftIdentCharSet of
    Just (None, None) -> parseChunks' @'None @'None txt
    Just (Alpha_, AlphaNum_') -> parseChunks' @'Alpha_ @'AlphaNum_' txt
    Just (Alpha_, AlphaNum_) -> parseChunks' @'Alpha_ @'AlphaNum_ txt
    Just (Alpha, AlphaNum_) -> parseChunks' @'Alpha @'AlphaNum_ txt
    Just (Alpha_, AlphaNumDash_) -> parseChunks' @'Alpha_ @'AlphaNumDash_ txt
    Just (AlphaDash_, AlphaNumDash_) -> parseChunks' @'AlphaDash_ @'AlphaNumDash_ txt
    Just (ClojureIdentStart, ClojureIdentCont) -> parseChunks' @'ClojureIdentStart @'ClojureIdentCont txt
    Just (CSharpIdentStart, CSharpIdentCont) -> parseChunks' @'CSharpIdentStart @'CSharpIdentCont txt
    Just (HtmlIdentStart, HtmlIdentCont) -> parseChunks' @'HtmlIdentStart @'HtmlIdentCont txt
    Just (JavaIdentStart, JavaIdentCont) -> parseChunks' @'JavaIdentStart @'JavaIdentCont txt
    Just (JuliaIdentStart, JuliaIdentCont) -> parseChunks' @'JuliaIdentStart @'JuliaIdentCont txt
    Just (ListIdent, ListIdent) -> parseChunks' @'ListIdent @'ListIdent txt
    Just (Unicode_, UnicodeNum_) -> parseChunks' @'Unicode_ @'UnicodeNum_ txt
    Just (UnicodeDollar_, UnicodeNumDollar_) -> parseChunks' @'UnicodeDollar_ @'UnicodeNumDollar_ txt
    Just (Unicode_, UnicodeNum_') -> parseChunks' @'Unicode_ @'UnicodeNum_' txt
    Just (Unicode_, UnicodeNumDollar_) -> parseChunks' @'Unicode_ @'UnicodeNumDollar_ txt
    Just (UnicodeXIDStart_, UnicodeNumXIDCont_) -> parseChunks' @'UnicodeXIDStart_ @'UnicodeNumXIDCont_ txt
    Just (AgdaIdent, AgdaIdent) -> parseChunks' @'AgdaIdent @'AgdaIdent txt
    charsets -> error $ "CGrep: unsupported CharSet combination: " <> show charsets


parseChunks' :: forall (cs1 :: CharSet) (csN :: CharSet) s. (IsCharSet cs1, IsCharSet csN) =>  T.Text -> ST s (S.Seq Chunk)
parseChunks' txt = do
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
                        if acc == Just (offset-1) &&
                            TU.unsafeHead (TU.dropWord8 (offset-1) txt) == '.'
                            then do stateR <~ StateDigit
                            else do stateR <~ StateDigit; accR <~ Just offset; tokensR <~ (tokens |> toChunk txt acc offset)
                    | isBracket' x -> do stateR <~ StateBracket; accR <~ Just offset; tokensR <~ (tokens |> toChunk txt acc offset)
                    | otherwise -> do stateR <~ StateOther;

    tokens <- readSTRef tokensR
    lastAcc <- readSTRef accR

    if lastAcc /= Just (TU.lengthWord8 txt)
        then do
            return $ tokens |> toChunk txt lastAcc (TU.lengthWord8 txt)
        else
            return $ tokens

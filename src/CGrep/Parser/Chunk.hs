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

module CGrep.Parser.Chunk (
    parseChunks,
    Chunk (..),
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

import CGrep.Parser.Char (
    isAlphaNum_,
    isAlpha_,
    isBracket',
    isCharNumber,
    isDigit,
    isSpace,
 )

import CGrep.FileTypeMap (FileTypeInfo (..))
import Data.List (genericLength)

import Data.MonoTraversable (MonoFoldable (oforM_))

import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import Data.Maybe (fromMaybe, fromJust)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (|>))
import qualified Data.Sequence as S
import Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.Text.Internal.StrictBuilder as T
import qualified Data.Text.Unsafe as TU

import Control.Monad (forM_)
import CGrep.Text (iterM)
import CGrep.Types (Offset(..))
import Data.Coerce (coerce)

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
    , cOffset :: {-# UNPACK #-} !Offset
    }
    deriving stock (Eq, Show, Ord)

data MatchLine = MatchLine
    { mlOffset :: {-# UNPACK #-} !Offset
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


toChunk :: T.Text -> Offset -> Maybe Int -> Int -> Chunk
toChunk _ _ Nothing _ = error "CGrep.Parser.Chunk.toChunk: Nothing (Internal Error)"
toChunk text charOff (Just beg) cur =
    let chunk = TU.takeWord8 (cur - beg) (TU.dropWord8 beg text)
       in Chunk ChunkUnspec chunk (charOff - (coerce $ T.length chunk))
{-# INLINE toChunk #-}


{-# COMPLETE StateSpace, StateAlpha, StateDigit, StateBracket, StateOther #-}

{-# INLINE parseChunks #-}
parseChunks :: Maybe FileTypeInfo -> T.Text -> S.Seq Chunk
parseChunks l t = runST $ case l >>= \FileTypeInfo{..} -> ftIdentCharSet of
    Just (isAlpha1, isAlphaN) -> parseChunks' isAlpha_ isAlphaNum_ t
    _ -> parseChunks' isAlpha_ isAlphaNum_ t
  where
    parseChunks' :: (Char -> Bool) -> (Char -> Bool) -> T.Text -> ST s (S.Seq Chunk)
    parseChunks' isAlpha1 isAlphaN txt = do
        stateR <- newSTRef StateSpace
        coffR <- newSTRef 0
        accR <- newSTRef Nothing
        tokensR <- newSTRef S.empty
        iterM txt $ \(# x, off #) -> do
            state <- readSTRef stateR
            coff <- readSTRef coffR
            acc <- readSTRef accR
            tokens <- readSTRef tokensR
            case state of
                StateSpace ->
                    if
                        | isSpace x -> do stateR <~ StateSpace; accR <~ Nothing
                        | isAlpha1 x -> do stateR <~ StateAlpha; accR <~ Just off
                        | isDigit x -> do stateR <~ StateDigit; accR <~ Just off
                        | isBracket' x -> do stateR <~ StateBracket; accR <~ Just off
                        | otherwise -> do stateR <~ StateOther; accR <~ Just off
                StateAlpha ->
                    if
                        | isAlphaN x -> do stateR <~ StateAlpha
                        | isSpace x -> do stateR <~ StateSpace; accR <~ Nothing; tokensR <~ (tokens |> toChunk txt coff acc off)
                        | isBracket' x -> do stateR <~ StateBracket; accR <~ Just off; tokensR <~ (tokens |> toChunk txt coff acc off)
                        | otherwise -> do stateR <~ StateOther; accR <~ Just off; tokensR <~ (tokens |> toChunk txt coff acc off)
                StateDigit ->
                    if
                        | isCharNumber x -> do stateR <~ StateDigit
                        | isSpace x -> do stateR <~ StateSpace; accR <~ Nothing; tokensR <~ (tokens |> toChunk txt coff acc off)
                        | isAlpha1 x -> do stateR <~ StateAlpha; accR <~ Just off; tokensR <~ (tokens |> toChunk txt coff acc off)
                        | isBracket' x -> do stateR <~ StateBracket; accR <~ Just off; tokensR <~ (tokens |> toChunk txt coff acc off)
                        | otherwise -> do stateR <~ StateOther; accR <~ Just off; tokensR <~ (tokens |> toChunk txt coff acc off)
                StateBracket ->
                    if
                        | isSpace x -> do stateR <~ StateSpace; accR <~ Nothing; tokensR <~ (tokens |> toChunk txt coff acc off)
                        | isAlpha1 x -> do stateR <~ StateAlpha; accR <~ Just off; tokensR <~ (tokens |> toChunk txt coff acc off)
                        | isDigit x -> do stateR <~ StateDigit; accR <~ Just off; tokensR <~ (tokens |> toChunk txt coff acc off)
                        | isBracket' x -> do stateR <~ StateBracket; accR <~ Just off; tokensR <~ (tokens |> toChunk txt coff acc off)
                        | otherwise -> do stateR <~ StateOther; accR <~ Just off; tokensR <~ (tokens |> toChunk txt coff acc off)
                StateOther ->
                    if
                        | isSpace x -> do stateR <~ StateSpace; accR <~ Nothing; tokensR <~ (tokens |> toChunk txt coff acc off)
                        | isAlpha1 x -> do stateR <~ StateAlpha; accR <~ Just off; tokensR <~ (tokens |> toChunk txt coff acc off)
                        | isDigit x ->
                            if acc == Just (off-1) &&
                                TU.unsafeHead (TU.dropWord8 (off-1) txt) == '.'
                                then do stateR <~ StateDigit
                                else do stateR <~ StateDigit; accR <~ Just off; tokensR <~ (tokens |> toChunk txt coff acc off)
                        | isBracket' x -> do stateR <~ StateBracket; accR <~ Just off; tokensR <~ (tokens |> toChunk txt coff acc off)
                        | otherwise -> do stateR <~ StateOther;
            coffR <~ (coff + 1)

        tokens <- readSTRef tokensR
        lastAcc <- readSTRef accR

        if lastAcc /= Just (TU.lengthWord8 txt)
            then do
                state <- readSTRef stateR
                coff <- readSTRef coffR
                return $ tokens |> toChunk txt coff lastAcc (TU.lengthWord8 txt)
            else
                return $ tokens

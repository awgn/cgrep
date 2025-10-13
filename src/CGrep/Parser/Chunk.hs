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
import CGrep.Types (Offset, Text8)
import Data.List (genericLength)

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as LB

import qualified ByteString.StrictBuilder as B

import Data.MonoTraversable (MonoFoldable (oforM_))

import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (|>))
import qualified Data.Sequence as S
import Data.Word (Word8)

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
    , cToken :: {-# UNPACK #-} !Text8
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

{-# COMPLETE StateSpace, StateAlpha, StateDigit, StateBracket, StateOther #-}

(<~) :: STRef s a -> a -> ST s ()
ref <~ !x = writeSTRef ref x
{-# INLINE (<~) #-}

{-# INLINE parseChunks #-}
parseChunks :: Maybe FileTypeInfo -> Text8 -> S.Seq Chunk
parseChunks l t = runST $ case l >>= \FileTypeInfo{..} -> ftIdentCharSet of
    Just (isAlpha1, isAlphaN) -> parseChunks' isAlpha_ isAlphaNum_ t
    _ -> parseChunks' isAlpha_ isAlphaNum_ t
  where
    parseChunks' :: (Char -> Bool) -> (Char -> Bool) -> C.ByteString -> ST s (S.Seq Chunk)
    parseChunks' isAlpha1 isAlphaN txt = do
        stateR <- newSTRef StateSpace
        offR <- newSTRef 0
        accR <- newSTRef (mempty :: B.Builder)
        tokensR <- newSTRef S.empty
        oforM_ txt $ \w -> do
            let x = BI.w2c w
            state <- readSTRef stateR
            off <- readSTRef offR
            acc <- readSTRef accR
            tokens <- readSTRef tokensR
            case state of
                StateSpace ->
                    if
                        | isSpace x -> do stateR <~ StateSpace; accR <~ mempty
                        | isAlpha1 x -> do stateR <~ StateAlpha; accR <~ B.asciiChar x
                        | isDigit x -> do stateR <~ StateDigit; accR <~ B.asciiChar x
                        | isBracket' x -> do stateR <~ StateBracket; accR <~ B.asciiChar x
                        | otherwise -> do stateR <~ StateOther; accR <~ B.asciiChar x
                StateAlpha ->
                    if
                        | isAlphaN x -> do stateR <~ StateAlpha; accR <~ (acc <> B.asciiChar x)
                        | isSpace x -> do stateR <~ StateSpace; accR <~ mempty; tokensR <~ (tokens |> toChunk off acc)
                        | isBracket' x -> do stateR <~ StateBracket; accR <~ B.asciiChar x; tokensR <~ (tokens |> toChunk off acc)
                        | otherwise -> do stateR <~ StateOther; accR <~ B.asciiChar x; tokensR <~ (tokens |> toChunk off acc)
                StateDigit ->
                    if
                        | isCharNumber x -> do stateR <~ StateDigit; accR <~ (acc <> B.asciiChar x)
                        | isSpace x -> do stateR <~ StateSpace; accR <~ mempty; tokensR <~ (tokens |> toChunk off acc)
                        | isAlpha1 x -> do stateR <~ StateAlpha; accR <~ B.asciiChar x; tokensR <~ (tokens |> toChunk off acc)
                        | isBracket' x -> do stateR <~ StateBracket; accR <~ B.asciiChar x; tokensR <~ (tokens |> toChunk off acc)
                        | otherwise -> do stateR <~ StateOther; accR <~ B.asciiChar x; tokensR <~ (tokens |> toChunk off acc)
                StateBracket ->
                    if
                        | isSpace x -> do stateR <~ StateSpace; accR <~ mempty; tokensR <~ (tokens |> toChunk off acc)
                        | isAlpha1 x -> do stateR <~ StateAlpha; accR <~ B.asciiChar x; tokensR <~ (tokens |> toChunk off acc)
                        | isDigit x -> do stateR <~ StateDigit; accR <~ B.asciiChar x; tokensR <~ (tokens |> toChunk off acc)
                        | isBracket' x -> do stateR <~ StateBracket; accR <~ B.asciiChar x; tokensR <~ (tokens |> toChunk off acc)
                        | otherwise -> do stateR <~ StateOther; accR <~ B.asciiChar x; tokensR <~ (tokens |> toChunk off acc)
                StateOther ->
                    if
                        | isSpace x -> do stateR <~ StateSpace; accR <~ mempty; tokensR <~ (tokens |> toChunk off acc)
                        | isAlpha1 x -> do stateR <~ StateAlpha; accR <~ B.asciiChar x; tokensR <~ (tokens |> toChunk off acc)
                        | isDigit x ->
                            if B.builderBytes acc == "."
                                then do stateR <~ StateDigit; accR <~ (acc <> B.asciiChar x)
                                else do stateR <~ StateDigit; accR <~ B.asciiChar x; tokensR <~ (tokens |> toChunk off acc)
                        | isBracket' x -> do stateR <~ StateBracket; accR <~ B.asciiChar x; tokensR <~ (tokens |> toChunk off acc)
                        | otherwise -> do stateR <~ StateOther; accR <~ (acc <> B.asciiChar x)
            offR <~ (off + 1)

        lastAcc <- readSTRef accR
        tokens <- readSTRef tokensR

        if B.builderLength lastAcc == 0
            then return tokens
            else do
                state <- readSTRef stateR
                off <- readSTRef offR
                return $ tokens |> toChunk off lastAcc

toChunk :: Offset -> B.Builder -> Chunk
toChunk off b = Chunk ChunkUnspec str (off - fromIntegral (B.builderLength b))
  where
    str = B.builderBytes b
{-# INLINE toChunk #-}

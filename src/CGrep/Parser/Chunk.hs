--
-- Copyright (c) 2013-2022 Nicola Bonelli <nicola@pfq.io>
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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module CGrep.Parser.Chunk (parseChunks) where

import qualified Data.DList as DL

import Data.Char
    ( isSpace, isAlphaNum, isDigit, isAlpha, isHexDigit )
import Data.Array.Unboxed ( (!), listArray, UArray )
import CGrep.Types ( Text8, LineOffset, Offset )
import Data.List (genericLength)
import CGrep.LanguagesMap ( LanguageInfo )

import CGrep.Chunk ( Chunk(..), Line(..) )

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B

data ChunkState =
    StateSpace   |
    StateAlpha   |
    StateDigit   |
    StateBracket |
    StateOther
        deriving (Eq, Enum, Show)


data ChunkAccum = ChunkAccum !ChunkState {-# UNPACK #-} !Offset B.Builder (DL.DList Chunk)

parseChunks :: Maybe LanguageInfo -> Text8 -> [Chunk]
parseChunks _ xs = (\(ChunkAccum _ off acc out) ->
    let accbs = toLazySmallByteString acc
      in DL.toList (if LB.null accbs
                    then out
                    else out `DL.snoc` mkChunk off acc)) $ C.foldl' tokens' (ChunkAccum StateSpace 0 mempty DL.empty) xs
    where tokens' :: ChunkAccum -> Char -> ChunkAccum
          tokens' (ChunkAccum StateSpace off _ out) x =
                if | isSpaceLT ! x      ->  ChunkAccum StateSpace   (off+1)  mempty         out
                   | isAlphaLT ! x      ->  ChunkAccum StateAlpha   (off+1) (B.char8  x) out
                   | isDigitLT ! x      ->  ChunkAccum StateDigit   (off+1) (B.char8  x) out
                   | isBracketLT ! x    ->  ChunkAccum StateBracket (off+1) (B.char8  x) out
                   | otherwise          ->  ChunkAccum StateOther   (off+1) (B.char8  x) out

          tokens' (ChunkAccum StateAlpha off acc out) x =
                if | isAlphaNumLT ! x   ->  ChunkAccum StateAlpha   (off+1) (acc <> B.char8 x)  out
                   | isSpaceLT ! x      ->  ChunkAccum StateSpace   (off+1)  mempty         (out `DL.snoc` mkChunk off acc)
                   | isBracketLT ! x    ->  ChunkAccum StateBracket (off+1) (B.char8  x) (out `DL.snoc` mkChunk off acc)
                   | otherwise          ->  ChunkAccum StateOther   (off+1) (B.char8  x) (out `DL.snoc` mkChunk off acc)

          tokens' (ChunkAccum StateDigit off acc out) x =
                if | isCharNumberLT ! x ->  ChunkAccum StateDigit   (off+1) (acc <> B.char8 x)  out
                   | isSpaceLT ! x      ->  ChunkAccum StateSpace   (off+1)  mempty         (out `DL.snoc` mkChunk off acc)
                   | isAlphaLT ! x      ->  ChunkAccum StateAlpha   (off+1) (B.char8  x) (out `DL.snoc` mkChunk off acc)
                   | isBracketLT ! x    ->  ChunkAccum StateBracket (off+1) (B.char8  x) (out `DL.snoc` mkChunk off acc)
                   | otherwise          ->  ChunkAccum StateOther   (off+1) (B.char8  x) (out `DL.snoc` mkChunk off acc)

          tokens' (ChunkAccum StateBracket off acc out) x =
                if | isSpaceLT ! x      ->  ChunkAccum StateSpace   (off+1)  mempty         (out `DL.snoc` mkChunk off acc)
                   | isAlphaLT ! x      ->  ChunkAccum StateAlpha   (off+1) (B.char8  x) (out `DL.snoc` mkChunk off acc)
                   | isDigitLT ! x      ->  ChunkAccum StateDigit   (off+1) (B.char8  x) (out `DL.snoc` mkChunk off acc)
                   | isBracketLT ! x    ->  ChunkAccum StateBracket (off+1) (B.char8  x) (out `DL.snoc` mkChunk off acc)
                   | otherwise          ->  ChunkAccum StateOther   (off+1) (B.char8  x) (out `DL.snoc` mkChunk off acc)

          tokens' (ChunkAccum StateOther off acc out) x =
                if | isSpaceLT ! x      ->  ChunkAccum StateSpace   (off+1)  mempty         (out `DL.snoc` mkChunk off acc)
                   | isAlphaLT ! x      ->  ChunkAccum StateAlpha   (off+1) (B.char8 x)  (out `DL.snoc` mkChunk off acc)
                   | isDigitLT ! x      ->  if toLazySmallByteString acc == "."
                                            then ChunkAccum StateDigit (off+1) (acc <> B.char8 x)  out
                                            else ChunkAccum StateDigit (off+1) (B.char8  x) (out `DL.snoc` mkChunk off acc)
                   | isBracketLT ! x    ->  ChunkAccum StateBracket    (off+1) (B.char8  x) (out `DL.snoc` mkChunk off acc)
                   | otherwise          ->  ChunkAccum StateOther      (off+1) (acc <> B.char8 x)  out


{-# NOINLINE toLazySmallByteString #-}
toLazySmallByteString :: B.Builder -> LB.ByteString
toLazySmallByteString =
  B.toLazyByteStringWith (B.safeStrategy 64 B.smallChunkSize) LB.empty


isCharNumberLT :: UArray Char Bool
isCharNumberLT =
    listArray ('\0', '\255')
        (map (\c -> isHexDigit c || c `elem` (".xX" :: String)) ['\0'..'\255'])
{-# INLINE isCharNumberLT #-}

isSpaceLT :: UArray Char Bool
isSpaceLT =
    listArray ('\0', '\255')
        (map isSpace ['\0'..'\255'])
{-# INLINE isSpaceLT #-}

isAlphaLT :: UArray Char Bool
isAlphaLT =
    listArray ('\0', '\255')
        (map (\c -> isAlpha c || c == '_') ['\0'..'\255'])
{-# INLINE isAlphaLT #-}

isAlphaNumLT :: UArray Char Bool
isAlphaNumLT =
    listArray ('\0', '\255')
        (map (\c -> isAlphaNum c || c == '_' || c == '\'') ['\0'..'\255'])
{-# INLINE isAlphaNumLT #-}

isDigitLT :: UArray Char Bool
isDigitLT =
    listArray ('\0', '\255')
        (map isDigit ['\0'..'\255'])
{-# INLINE isDigitLT #-}

isBracketLT :: UArray Char Bool
isBracketLT =
    listArray ('\0', '\255')
        (map (`elem` ("{[()]}" :: String)) ['\0'..'\255'])
{-# INLINE isBracketLT #-}


mkChunk :: Offset -> B.Builder -> Chunk
mkChunk off b =  Chunk (off - fromIntegral (C.length str)) str
    where str = LB.toStrict $ toLazySmallByteString b
{-# INLINE mkChunk #-}

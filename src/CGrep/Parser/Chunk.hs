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
{-# LANGUAGE RecordWildCards #-}

module CGrep.Parser.Chunk (parseChunks) where

import qualified Data.DList as DL


import CGrep.Parser.Char

import CGrep.Types ( Text8, Offset )
import Data.List (genericLength)
import CGrep.LanguagesMap

import CGrep.Chunk ( Chunk(..), MatchingLine(..), mkChunk )

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Internal as BI

import qualified ByteString.StrictBuilder as B

import Data.MonoTraversable ( MonoFoldable(oforM_) )

import Data.STRef ( STRef, newSTRef, writeSTRef, readSTRef )
import Control.Monad.ST ( ST, runST )

import CGrep.Parser.Char ( isCharNumber, isBracket', isAlpha_, isAlphaNum_ )

import qualified Data.Sequence as S
import Data.Sequence ((|>), Seq((:<|), (:|>), Empty))
import Data.Maybe ( fromMaybe )

data ChunkState =
    StateSpace   |
    StateAlpha   |
    StateDigit   |
    StateBracket |
    StateOther
        deriving (Eq, Enum, Show)


(<~) :: STRef s a -> a -> ST s ()
ref <~ !x = writeSTRef ref x
{-# INLINE (<~) #-}

{-# INLINE parseChunks #-}
parseChunks :: Maybe LanguageInfo -> Text8 -> S.Seq Chunk
parseChunks l t = runST $ case l >>= \LanguageInfo {..} -> langIdentifierChars of
        Just (isAlpha1, isAlphaN) -> parseChunks' isAlpha_ isAlphaNum_ t
        _                         -> parseChunks' isAlpha_ isAlphaNum_ t
  where parseChunks' :: (Char->Bool) -> (Char -> Bool) -> C.ByteString -> ST a (S.Seq Chunk)
        parseChunks' isAlpha1 isAlphaN txt  = do
          stateR  <- newSTRef StateSpace
          offR    <- newSTRef 0
          accR    <- newSTRef (mempty :: B.Builder)
          tokensR <- newSTRef S.empty
          oforM_ txt $ \w -> do
            let x = BI.w2c w
            state  <- readSTRef stateR
            off    <- readSTRef offR
            acc    <- readSTRef accR
            tokens <- readSTRef tokensR
            case state of
                StateSpace ->
                    if | isSpace x        ->  do stateR <~ StateSpace   ; accR <~ mempty
                       | isAlpha1   x     ->  do stateR <~ StateAlpha   ; accR <~ B.asciiChar  x
                       | isDigit x        ->  do stateR <~ StateDigit   ; accR <~ B.asciiChar  x
                       | isBracket'  x    ->  do stateR <~ StateBracket ; accR <~ B.asciiChar  x
                       | otherwise        ->  do stateR <~ StateOther   ; accR <~ B.asciiChar  x
                StateAlpha ->
                    if | isAlphaN   x     ->  do stateR <~ StateAlpha   ; accR <~ (acc <> B.asciiChar x)
                       | isSpace x        ->  do stateR <~ StateSpace   ; accR <~ mempty         ; tokensR <~ (tokens |> toChunk off acc)
                       | isBracket'  x    ->  do stateR <~ StateBracket ; accR <~ B.asciiChar  x ; tokensR <~ (tokens |> toChunk off acc)
                       | otherwise        ->  do stateR <~ StateOther   ; accR <~ B.asciiChar  x ; tokensR <~ (tokens |> toChunk off acc)
                StateDigit ->
                    if | isCharNumber   x ->  do stateR <~ StateDigit   ; accR <~ (acc <> B.asciiChar x)
                       | isSpace  x       ->  do stateR <~ StateSpace   ; accR <~  mempty        ; tokensR <~ (tokens |> toChunk off acc)
                       | isAlpha1    x    ->  do stateR <~ StateAlpha   ; accR <~ B.asciiChar  x ; tokensR <~ (tokens |> toChunk off acc)
                       | isBracket'  x    ->  do stateR <~ StateBracket ; accR <~ B.asciiChar  x ; tokensR <~ (tokens |> toChunk off acc)
                       | otherwise        ->  do stateR <~ StateOther   ; accR <~ B.asciiChar  x ; tokensR <~ (tokens |> toChunk off acc)
                StateBracket ->
                    if | isSpace  x       ->  do stateR <~ StateSpace   ; accR <~  mempty       ; tokensR <~ (tokens |> toChunk off acc)
                       | isAlpha1    x    ->  do stateR <~ StateAlpha   ; accR <~ B.asciiChar x ; tokensR <~ (tokens |> toChunk off acc)
                       | isDigit  x       ->  do stateR <~ StateDigit   ; accR <~ B.asciiChar x ; tokensR <~ (tokens |> toChunk off acc)
                       | isBracket' x     ->  do stateR <~ StateBracket ; accR <~ B.asciiChar x ; tokensR <~ (tokens |> toChunk off acc)
                       | otherwise        ->  do stateR <~ StateOther   ; accR <~ B.asciiChar x ; tokensR <~ (tokens |> toChunk off acc)
                StateOther ->
                    if | isSpace  x       ->  do stateR <~ StateSpace   ; accR <~ mempty        ; tokensR <~ (tokens |> toChunk off acc)
                       | isAlpha1   x     ->  do stateR <~ StateAlpha   ; accR <~ B.asciiChar x ; tokensR <~ (tokens |> toChunk off acc)
                       | isDigit  x       ->  if B.builderBytes acc == "."
                                                then do stateR <~ StateDigit ; accR <~ (acc <> B.asciiChar x)
                                                else do stateR <~ StateDigit ; accR <~ B.asciiChar  x ; tokensR <~ (tokens |> toChunk off acc)
                       | isBracket'  x    ->  do stateR <~ StateBracket ; accR <~ B.asciiChar x       ; tokensR <~ (tokens |> toChunk off acc)
                       | otherwise        ->  do stateR <~ StateOther   ; accR <~ (acc <> B.asciiChar x)
            offR <~ (off+1)

          lastAcc <- readSTRef accR
          tokens  <- readSTRef tokensR

          if B.builderLength lastAcc == 0
              then return tokens
              else do
                state   <- readSTRef stateR
                off     <- readSTRef offR
                return $ tokens |> toChunk off lastAcc


toChunk :: Offset -> B.Builder -> Chunk
toChunk off b =  mkChunk str (off - fromIntegral (B.builderLength b))
    where str = B.builderBytes b
{-# INLINE toChunk #-}

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

import CGrep.Types ( Text8, LineOffset, Offset )
import Data.List (genericLength)
import CGrep.LanguagesMap ( LanguageInfo )

import CGrep.Chunk ( Chunk(..), Line(..) )

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Internal as BI

import qualified ByteString.StrictBuilder as B

import Data.MonoTraversable ( MonoFoldable(oforM_) )

import Data.STRef ( STRef, newSTRef, writeSTRef, readSTRef )
import Control.Monad.ST ( ST, runST )

import CGrep.Parser.Char
    ( isCharNumberC, isAlphaC, isAlphaNumC, isBracketC )

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


parseChunks :: Maybe LanguageInfo -> Text8 -> [Chunk]
parseChunks l t = runST $ parseChunks' l t
  where parseChunks' :: Maybe LanguageInfo -> C.ByteString -> ST a [Chunk]
        parseChunks' linfo txt  = do
          stateR  <- newSTRef StateSpace
          offR    <- newSTRef 0
          skipR   <- newSTRef 0
          accR    <- newSTRef (mempty :: B.Builder)
          tokensR <- newSTRef DL.empty
          oforM_ txt $ \w -> do
            let x = BI.w2c w
            state  <- readSTRef stateR
            off    <- readSTRef offR
            skip   <- readSTRef skipR
            acc    <- readSTRef accR
            tokens <- readSTRef tokensR
            case state of
                StateSpace ->
                    if | isSpace x        ->  do stateR <~ StateSpace   ; accR <~  mempty
                       | isAlphaC x       ->  do stateR <~ StateAlpha   ; accR <~ B.asciiChar  x
                       | isDigit x        ->  do stateR <~ StateDigit   ; accR <~ B.asciiChar  x
                       | isBracketC  x    ->  do stateR <~ StateBracket ; accR <~ B.asciiChar  x
                       | otherwise        ->  do stateR <~ StateOther   ; accR <~ B.asciiChar  x
                StateAlpha ->
                    if | isAlphaNumC  x   ->  do stateR <~ StateAlpha   ; accR <~ (acc <> B.asciiChar x)
                       | isSpace x        ->  do stateR <~ StateSpace   ; accR <~ mempty     ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                       | isBracketC  x    ->  do stateR <~ StateBracket ; accR <~ B.asciiChar  x ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                       | otherwise        ->  do stateR <~ StateOther   ; accR <~ B.asciiChar  x ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                StateDigit ->
                    if | isCharNumberC  x ->  do stateR <~ StateDigit   ; accR <~ (acc <> B.asciiChar x)
                       | isSpace  x       ->  do stateR <~ StateSpace   ; accR <~  mempty    ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                       | isAlphaC  x      ->  do stateR <~ StateAlpha   ; accR <~ B.asciiChar  x ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                       | isBracketC  x    ->  do stateR <~ StateBracket ; accR <~ B.asciiChar  x ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                       | otherwise        ->  do stateR <~ StateOther   ; accR <~ B.asciiChar  x ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                StateBracket ->
                    if | isSpace  x       ->  do stateR <~ StateSpace   ; accR <~  mempty   ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                       | isAlphaC  x      ->  do stateR <~ StateAlpha   ; accR <~ B.asciiChar x ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                       | isDigit  x       ->  do stateR <~ StateDigit   ; accR <~ B.asciiChar x ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                       | isBracketC x     ->  do stateR <~ StateBracket ; accR <~ B.asciiChar x ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                       | otherwise        ->  do stateR <~ StateOther   ; accR <~ B.asciiChar x ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                StateOther ->
                    if | isSpace  x       ->  do stateR <~ StateSpace   ; accR <~ mempty    ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                       | isAlphaC  x      ->  do stateR <~ StateAlpha   ; accR <~ B.asciiChar x ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                       | isDigit  x       ->  if B.builderBytes acc == "."
                                                then do stateR <~ StateDigit ; accR <~ (acc <> B.asciiChar x)
                                                else do stateR <~ StateDigit ; accR <~ B.asciiChar  x; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                       | isBracketC  x    ->  do stateR <~ StateBracket ; accR <~ B.asciiChar x ; tokensR <~ (tokens `DL.snoc` mkChunk off acc)
                       | otherwise        ->  do stateR <~ StateOther   ; accR <~ (acc <> B.asciiChar x)
            offR <~ (off+1)

          lastAcc <- readSTRef accR
          tokens  <- readSTRef tokensR

          DL.toList <$>
            if B.builderLength lastAcc == 0
                then return tokens
                else do
                  state   <- readSTRef stateR
                  off     <- readSTRef offR
                  return $ tokens `DL.snoc` mkChunk off lastAcc


mkChunk :: Offset -> B.Builder -> Chunk
mkChunk off b =  Chunk (off - fromIntegral (B.builderLength b)) str
    where str = B.builderBytes b
{-# INLINE mkChunk #-}

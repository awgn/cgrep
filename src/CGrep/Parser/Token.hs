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

module CGrep.Parser.Token (tokenizer) where

import qualified Data.ByteString.Char8 as C
import qualified Data.DList as DL

import Data.Char
    ( isSpace, isAlphaNum, isDigit, isAlpha, isHexDigit )
import Data.Array.Unboxed ( (!), listArray, UArray )
import CGrep.Types ( Text8, LineOffset, Offset )
import Data.List (genericLength)
import CGrep.LanguagesMap ( LanguageInfo )

import CGrep.Chunk ( Chunk(..), Line(..) )

type DString = DL.DList Char

data TokenState =
    StateSpace   |
    StateAlpha   |
    StateDigit   |
    StateBracket |
    StateOther
        deriving (Eq, Enum, Show)


data TokenAccum = TokenAccum !TokenState {-# UNPACK #-} !Offset DString (DL.DList Chunk)


isCharNumberLT :: UArray Char Bool
isCharNumberLT =
    listArray ('\0', '\255')
        (map (\c -> isHexDigit c || c `elem` ".xX") ['\0'..'\255'])
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
        (map (`elem` "{[()]}") ['\0'..'\255'])
{-# INLINE isBracketLT #-}


mkChunk :: Offset -> DString -> Chunk
mkChunk off ds =  Chunk (off - genericLength str) (C.pack str)
    where str = DL.toList ds
{-# INLINE mkChunk #-}


tokenizer :: Maybe LanguageInfo -> Text8 -> [Chunk]
tokenizer _ xs = (\(TokenAccum _ off acc out) ->
      DL.toList (if null (DL.toList acc) then out
                                         else out `DL.snoc` mkChunk off acc)) $
                                            C.foldl' tokens' (TokenAccum StateSpace 0 DL.empty DL.empty) xs
    where tokens' :: TokenAccum -> Char -> TokenAccum
          tokens' (TokenAccum StateSpace off _ out) x =
                if | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1)  DL.empty         out
                   | isAlphaLT ! x      ->  TokenAccum StateAlpha   (off+1) (DL.singleton  x) out
                   | isDigitLT ! x      ->  TokenAccum StateDigit   (off+1) (DL.singleton  x) out
                   | isBracketLT ! x    ->  TokenAccum StateBracket (off+1) (DL.singleton  x) out
                   | otherwise          ->  TokenAccum StateOther   (off+1) (DL.singleton  x) out

          tokens' (TokenAccum StateAlpha off acc out) x =
                if | isAlphaNumLT ! x   ->  TokenAccum StateAlpha   (off+1) (acc `DL.snoc` x)  out
                   | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1)  DL.empty         (out `DL.snoc` mkChunk off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket (off+1) (DL.singleton  x) (out `DL.snoc` mkChunk off acc)
                   | otherwise          ->  TokenAccum StateOther   (off+1) (DL.singleton  x) (out `DL.snoc` mkChunk off acc)

          tokens' (TokenAccum StateDigit off acc out) x =
                if | isCharNumberLT ! x ->  TokenAccum StateDigit   (off+1) (acc `DL.snoc` x)  out
                   | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1)  DL.empty         (out `DL.snoc` mkChunk off acc)
                   | isAlphaLT ! x      ->  TokenAccum StateAlpha   (off+1) (DL.singleton  x) (out `DL.snoc` mkChunk off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket (off+1) (DL.singleton  x) (out `DL.snoc` mkChunk off acc)
                   | otherwise          ->  TokenAccum StateOther   (off+1) (DL.singleton  x) (out `DL.snoc` mkChunk off acc)

          tokens' (TokenAccum StateBracket off acc out) x =
                if | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1)  DL.empty         (out `DL.snoc` mkChunk off acc)
                   | isAlphaLT ! x      ->  TokenAccum StateAlpha   (off+1) (DL.singleton  x) (out `DL.snoc` mkChunk off acc)
                   | isDigitLT ! x      ->  TokenAccum StateDigit   (off+1) (DL.singleton  x) (out `DL.snoc` mkChunk off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket (off+1) (DL.singleton  x) (out `DL.snoc` mkChunk off acc)
                   | otherwise          ->  TokenAccum StateOther   (off+1) (DL.singleton  x) (out `DL.snoc` mkChunk off acc)

          tokens' (TokenAccum StateOther off acc out) x =
                if | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1)  DL.empty         (out `DL.snoc` mkChunk off acc)
                   | isAlphaLT ! x      ->  TokenAccum StateAlpha   (off+1) (DL.singleton x)  (out `DL.snoc` mkChunk off acc)
                   | isDigitLT ! x      ->  if DL.toList acc == "."
                                            then TokenAccum StateDigit (off+1) (acc `DL.snoc` x)  out
                                            else TokenAccum StateDigit (off+1) (DL.singleton  x) (out `DL.snoc` mkChunk off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket    (off+1) (DL.singleton  x) (out `DL.snoc` mkChunk off acc)
                   | otherwise          ->  TokenAccum StateOther      (off+1) (acc `DL.snoc` x)  out

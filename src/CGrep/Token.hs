--
-- Copyright (c) 2013 Bonelli Nicola <bonelli@antifork.org>
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


module CGrep.Token (Token, MatchLine, tokens, tokenizer) where

import qualified Data.ByteString.Char8 as C
import qualified Data.DList as DL

import Data.Char
import Data.Array.Unboxed
import CGrep.Types


type Token      = (Offset, String)
type MatchLine  = (OffsetLine, [Token])
type DString    = DL.DList Char


data TokenState =
    StateSpace   |
    StateAlpha   |
    StateDigit   |
    StateBracket |
    StateOther
        deriving (Eq, Enum, Show)


data TokenAccum = TokenAccum !TokenState !Offset DString (DL.DList Token)


isCharNumberLT :: UArray Char Bool
isCharNumberLT =
    listArray ('\0', '\255')
        (map (\c -> isHexDigit c || c `elem` ".xX") ['\0'..'\255'])


isSpaceLT :: UArray Char Bool
isSpaceLT =
    listArray ('\0', '\255')
        (map isSpace ['\0'..'\255'])

isAlphaLT :: UArray Char Bool
isAlphaLT =
    listArray ('\0', '\255')
        (map (\c -> isAlpha c || c == '_') ['\0'..'\255'])

isAlphaNumLT :: UArray Char Bool
isAlphaNumLT =
    listArray ('\0', '\255')
        (map (\c -> isAlphaNum c || c == '_' || c == '\'') ['\0'..'\255'])

isDigitLT :: UArray Char Bool
isDigitLT =
    listArray ('\0', '\255')
        (map isDigit ['\0'..'\255'])

isBracketLT :: UArray Char Bool
isBracketLT =
    listArray ('\0', '\255')
        (map (`elem` "{[()]}") ['\0'..'\255'])


{-# INLINE mkToken #-}

mkToken :: Offset -> DString -> Token
mkToken off ds =  (off - length str, str)
    where str = DL.toList ds


tokens :: Text8 -> [String]
tokens = map snd . tokenizer


tokenizer :: Text8 -> [Token]
tokenizer xs = (\(TokenAccum _ off acc out) -> DL.toList (if null (DL.toList acc) then out
                                                                                  else out `DL.snoc` mkToken off acc)) $ C.foldl' tokens' (TokenAccum StateSpace 0 DL.empty DL.empty) xs
    where tokens' :: TokenAccum -> Char -> TokenAccum
          tokens' (TokenAccum StateSpace off _ out) x =
              case () of
                _  | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1)  DL.empty         out
                   | isAlphaLT ! x      ->  TokenAccum StateAlpha   (off+1) (DL.singleton  x) out
                   | isDigitLT ! x      ->  TokenAccum StateDigit   (off+1) (DL.singleton  x) out
                   | isBracketLT ! x    ->  TokenAccum StateBracket (off+1) (DL.singleton  x) out
                   | otherwise          ->  TokenAccum StateOther   (off+1) (DL.singleton  x) out

          tokens' (TokenAccum StateAlpha off acc out) x =
              case () of
                _  | isAlphaNumLT ! x   ->  TokenAccum StateAlpha   (off+1) (acc `DL.snoc` x)  out
                   | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1)  DL.empty         (out `DL.snoc` mkToken off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket (off+1) (DL.singleton  x) (out `DL.snoc` mkToken off acc)
                   | otherwise          ->  TokenAccum StateOther   (off+1) (DL.singleton  x) (out `DL.snoc` mkToken off acc)

          tokens' (TokenAccum StateDigit off acc out) x =
              case () of
                _  | isCharNumberLT ! x ->  TokenAccum StateDigit   (off+1) (acc `DL.snoc` x)  out
                   | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1)  DL.empty         (out `DL.snoc` mkToken off acc)
                   | isAlphaLT ! x      ->  TokenAccum StateAlpha   (off+1) (DL.singleton  x) (out `DL.snoc` mkToken off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket (off+1) (DL.singleton  x) (out `DL.snoc` mkToken off acc)
                   | otherwise          ->  TokenAccum StateOther   (off+1) (DL.singleton  x) (out `DL.snoc` mkToken off acc)

          tokens' (TokenAccum StateBracket off acc out) x =
              case () of
                _  | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1)  DL.empty         (out `DL.snoc` mkToken off acc)
                   | isAlphaLT ! x      ->  TokenAccum StateAlpha   (off+1) (DL.singleton  x) (out `DL.snoc` mkToken off acc)
                   | isDigitLT ! x      ->  TokenAccum StateDigit   (off+1) (DL.singleton  x) (out `DL.snoc` mkToken off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket (off+1) (DL.singleton  x) (out `DL.snoc` mkToken off acc)
                   | otherwise          ->  TokenAccum StateOther   (off+1) (DL.singleton  x) (out `DL.snoc` mkToken off acc)

          tokens' (TokenAccum StateOther off acc out) x =
              case () of
                _  | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1)  DL.empty         (out `DL.snoc` mkToken off acc)
                   | isAlphaLT ! x      ->  TokenAccum StateAlpha   (off+1) (DL.singleton x)  (out `DL.snoc` mkToken off acc)
                   | isDigitLT ! x      ->  if DL.toList acc == "."
                                            then TokenAccum StateDigit (off+1) (acc `DL.snoc` x)  out
                                            else TokenAccum StateDigit (off+1) (DL.singleton  x) (out `DL.snoc` mkToken off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket    (off+1) (DL.singleton  x) (out `DL.snoc` mkToken off acc)
                   | otherwise          ->  TokenAccum StateOther      (off+1) (acc `DL.snoc` x)  out


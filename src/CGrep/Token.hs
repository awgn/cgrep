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


module CGrep.Token (tokens, tokenizer, isCompleteToken) where

import qualified Data.ByteString.Char8 as C

import Data.Char
import CGrep.Types


data TokenState = TokenSpace |
                  TokenAlpha |
                  TokenDigit |
                  TokenOther
                    deriving (Eq, Enum, Show)


data TokenAccum = TokenAccum !TokenState !Offset String [Token]
    deriving (Show,Eq)


isCharNumber :: Char -> Bool
isCharNumber c = isHexDigit c || c == '.' || c == 'x' || c == 'X'


isCompleteToken:: Text8 -> (Offset, String) -> Bool
isCompleteToken text (off, tok) = tok `elem` ts
    where ts = tokens $ C.take (length tok + extra + 2) $ C.drop (off - extra) text
          extra = 10


tokens :: Text8 -> [String]
tokens = map snd . tokenizer


{-# INLINE mkToken #-}

mkToken :: Offset -> String -> Token
mkToken off acc =  (off - length acc, reverse acc)


tokenizer :: Text8 -> [Token]
tokenizer xs = (\(TokenAccum _  off acc out) -> if null acc then out else reverse out ++ [mkToken off acc]) $ C.foldl' tokens' (TokenAccum TokenSpace 0 "" []) xs
    where tokens' :: TokenAccum -> Char -> TokenAccum
          tokens' (TokenAccum TokenSpace off acc out) x =
              case () of
                _  | isSpace x                ->  TokenAccum TokenSpace (off+1) acc out
                   | isAlpha x || x == '_'    ->  TokenAccum TokenAlpha (off+1) (x : acc) out
                   | isDigit x                ->  TokenAccum TokenDigit (off+1) (x : acc) out
                   | otherwise                ->  TokenAccum TokenOther (off+1) (x : acc) out

          tokens' (TokenAccum TokenAlpha off acc out) x =
              case () of
                _  | isSpace x                ->  TokenAccum TokenSpace (off+1) ""  (mkToken off acc : out)
                   | isAlphaNum x || x == '_' ->  TokenAccum TokenAlpha (off+1) (x : acc) out
                   | otherwise                ->  TokenAccum TokenOther (off+1) [x] (mkToken off acc : out)

          tokens' (TokenAccum TokenDigit off acc out) x =
              case () of
                _  | isSpace x                ->  TokenAccum TokenSpace (off+1) ""  (mkToken off acc : out)
                   | isCharNumber x           ->  TokenAccum TokenDigit (off+1) (x : acc) out
                   | isAlpha x || x == '_'    ->  TokenAccum TokenAlpha (off+1) [x] (mkToken off acc : out)
                   | otherwise                ->  TokenAccum TokenOther (off+1) [x] (mkToken off acc : out)

          tokens' (TokenAccum TokenOther off acc out) x =
              case () of
                _  | isSpace x                ->  TokenAccum TokenSpace (off+1) ""  (mkToken off acc : out)
                   | isAlpha x || x == '_'    ->  TokenAccum TokenAlpha (off+1) [x] (mkToken off acc : out)
                   | isDigit x                ->  if acc == "." then TokenAccum TokenDigit (off+1) (x : ".") out
                                                                else TokenAccum TokenDigit (off+1) [x] (mkToken off acc : out)
                   | otherwise                ->  TokenAccum TokenOther (off+1) (x : acc) out




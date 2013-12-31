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

module CGrep.Token where

import qualified Data.ByteString.Char8 as C

import Data.Char
import CGrep.Types

data TokenState = TokenSpace |
                  TokenAlpha |
                  TokenDigit |
                  TokenOther 
                    deriving (Eq, Enum, Show)


isCharNumber :: Char -> Bool
isCharNumber c = isHexDigit c || c == '.' || c == 'x' || c == 'X' 


isCompleteToken:: C.ByteString -> (Offset, String) -> Bool
isCompleteToken text (off, tok) = tok `elem` ts
    where ts = tokens $ C.take (length tok + extra + 2) $ C.drop (off - extra) text 
          extra = 10               
                                 

data TokenAccum = TokenAccum TokenState String [String]
    deriving (Show,Eq)


tokens :: C.ByteString -> [String]
tokens xs = (\(TokenAccum _ acc out) -> if null acc then out else reverse out ++ [reverse acc]) $ C.foldl' tokens' (TokenAccum TokenSpace "" []) xs
    where tokens' :: TokenAccum -> Char -> TokenAccum
          tokens' (TokenAccum TokenSpace acc out) x =  
              case () of
                _  | isSpace x                ->  TokenAccum TokenSpace acc out
                   | isAlpha x || x == '_'    ->  TokenAccum TokenAlpha (x : acc) out
                   | isDigit x                ->  TokenAccum TokenDigit (x : acc) out
                   | otherwise                ->  TokenAccum TokenOther (x : acc) out
       
          tokens' (TokenAccum TokenAlpha acc out) x = 
              case () of
                _  | isSpace x                ->  TokenAccum TokenSpace  "" (reverse acc : out)
                   | isAlphaNum x || x == '_' ->  TokenAccum TokenAlpha  (x : acc) out
                   | otherwise                ->  TokenAccum TokenOther  [x] (reverse acc : out) 
       
          tokens' (TokenAccum TokenDigit acc out) x = 
              case () of
                _  | isSpace x                ->  TokenAccum TokenSpace "" (reverse acc : out)
                   | isCharNumber x           ->  TokenAccum TokenDigit (x : acc) out
                   | isAlpha x || x == '_'    ->  TokenAccum TokenAlpha [x] (reverse acc : out) 
                   | otherwise                ->  TokenAccum TokenOther [x] (reverse acc : out) 
       
          tokens' (TokenAccum TokenOther acc out) x = 
              case () of
                _  | isSpace x                ->  TokenAccum TokenSpace ""  (reverse acc : out)   
                   | isAlpha x || x == '_'    ->  TokenAccum TokenAlpha [x] (reverse acc : out)
                   | isDigit x                ->  if acc == "." then TokenAccum TokenDigit (x : ".") out 
                                                                else TokenAccum TokenDigit [x] (reverse acc : out)
                   | otherwise                ->  TokenAccum TokenOther (x : acc) out 




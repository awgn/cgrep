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


module CGrep.Semantic.Generic.Token (tokenizer, Token(..)) where

import qualified Data.ByteString.Char8 as C
import qualified Data.DList as DL

import Data.Char
import Data.Array.Unboxed

import CGrep.Semantic.Token
import CGrep.Types

type DString = DL.DList Char


data TokenState = StateSpace   |
                  StateAlpha   |
                  StateDigit   |
                  StateBracket |
                  StateOther
                    deriving (Eq, Enum, Show)


data Token = TokenAlpha       { toString :: String, offset :: Offset  } |
             TokenDigit       { toString :: String, offset :: Offset  } |
             TokenBracket     { toString :: String, offset :: Offset  } |
             TokenOther       { toString :: String, offset :: Offset  }
                deriving (Show, Eq, Ord)

isTokenAlpha, isTokenDigit, isTokenBracket, isTokenOther :: Token -> Bool

isTokenAlpha (TokenAlpha _ _) = True
isTokenAlpha _  = False

isTokenDigit (TokenDigit _ _) = True
isTokenDigit _  = False

isTokenBracket (TokenBracket _ _) = True
isTokenBracket _  = False

isTokenOther (TokenOther _ _) = True
isTokenOther _  = False


tokenCompare :: Token -> Token -> Bool
tokenCompare (TokenAlpha   { toString = l }) (TokenAlpha   { toString = r }) = l == r
tokenCompare (TokenDigit   { toString = l }) (TokenDigit   { toString = r }) = l == r
tokenCompare (TokenBracket { toString = l }) (TokenBracket { toString = r }) = l == r
tokenCompare (TokenOther   { toString = l }) (TokenOther   { toString = r }) = l == r
tokenCompare _ _ = False


data TokenAccum = TokenAccum !TokenState !Offset DString (DL.DList Token)


isCharNumberLT :: UArray Char Bool
isCharNumberLT =
    listArray ('\0', '\255')
        (map (\c -> isHexDigit c || c `elem` ['.', 'x','X']) ['\0'..'\255'])


isSpaceLT :: UArray Char Bool
isSpaceLT =
    listArray ('\0', '\255')
        (map (\c -> isSpace c) ['\0'..'\255'])

isAlphaLT :: UArray Char Bool
isAlphaLT =
    listArray ('\0', '\255')
        (map (\c -> isAlpha c || c == '_') ['\0'..'\255'])

isAlphaNumLT :: UArray Char Bool
isAlphaNumLT =
    listArray ('\0', '\255')
        (map (\c -> isAlphaNum c || c == '_') ['\0'..'\255'])

isDigitLT :: UArray Char Bool
isDigitLT =
    listArray ('\0', '\255')
        (map (\c -> isDigit c) ['\0'..'\255'])

isBracketLT :: UArray Char Bool
isBracketLT =
    listArray ('\0', '\255')
        (map (\c -> c `elem` "{[()]}" ) ['\0'..'\255'])


{-# INLINE mkToken #-}


mkToken :: (String -> Offset -> Token) -> Offset -> DString -> Token
mkToken ctor off ds =  ctor str (off - length str)
    where str = DL.toList ds


mkTokenCtor :: TokenState -> (String -> Offset -> Token)
mkTokenCtor StateSpace   = TokenOther
mkTokenCtor StateAlpha   = TokenAlpha
mkTokenCtor StateDigit   = TokenDigit
mkTokenCtor StateBracket = TokenBracket
mkTokenCtor StateOther   = TokenOther


tokenizer :: Text8 -> [Token]
tokenizer xs = (\(TokenAccum ss  off acc out) -> DL.toList (out `DL.snoc` mkToken (mkTokenCtor ss) off acc)) $ C.foldl' tokens' (TokenAccum StateSpace 0 DL.empty DL.empty) xs
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
                   | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1)  DL.empty         (out `DL.snoc` mkToken TokenAlpha off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket (off+1) (DL.singleton  x) (out `DL.snoc` mkToken TokenAlpha off acc)
                   | otherwise          ->  TokenAccum StateOther   (off+1) (DL.singleton  x) (out `DL.snoc` mkToken TokenAlpha off acc)

          tokens' (TokenAccum StateDigit off acc out) x =
              case () of
                _  | isCharNumberLT ! x ->  TokenAccum StateDigit   (off+1) (acc `DL.snoc` x)  out
                   | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1)  DL.empty         (out `DL.snoc` mkToken TokenDigit off acc)
                   | isAlphaLT ! x      ->  TokenAccum StateAlpha   (off+1) (DL.singleton  x) (out `DL.snoc` mkToken TokenDigit off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket (off+1) (DL.singleton  x) (out `DL.snoc` mkToken TokenDigit off acc)
                   | otherwise          ->  TokenAccum StateOther   (off+1) (DL.singleton  x) (out `DL.snoc` mkToken TokenDigit off acc)

          tokens' (TokenAccum StateBracket off acc out) x =
              case () of
                _  | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1)  DL.empty         (out `DL.snoc` mkToken TokenBracket off acc)
                   | isAlphaLT ! x      ->  TokenAccum StateAlpha   (off+1) (DL.singleton  x) (out `DL.snoc` mkToken TokenBracket off acc)
                   | isDigitLT ! x      ->  TokenAccum StateDigit   (off+1) (DL.singleton  x) (out `DL.snoc` mkToken TokenBracket off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket (off+1) (DL.singleton  x) (out `DL.snoc` mkToken TokenBracket off acc)
                   | otherwise          ->  TokenAccum StateOther   (off+1) (DL.singleton  x) (out `DL.snoc` mkToken TokenBracket off acc)

          tokens' (TokenAccum StateOther off acc out) x =
              case () of
                _  | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1)  DL.empty         (out `DL.snoc` mkToken TokenOther off acc)
                   | isAlphaLT ! x      ->  TokenAccum StateAlpha   (off+1) (DL.singleton x)  (out `DL.snoc` mkToken TokenOther off acc)
                   | isDigitLT ! x      ->  if DL.toList acc == "."
                                            then TokenAccum StateDigit (off+1) (acc `DL.snoc` x)  out
                                            else TokenAccum StateDigit (off+1) (DL.singleton  x) (out `DL.snoc` mkToken TokenOther off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket    (off+1) (DL.singleton  x) (out `DL.snoc` mkToken TokenOther off acc)
                   | otherwise          ->  TokenAccum StateOther      (off+1) (acc `DL.snoc` x)  out


instance SemanticToken Token where
    tkIsIdentifier  = isTokenAlpha
    tkIsString      = \_ -> False
    tkIsChar        = \_ -> False
    tkIsNumber      = isTokenDigit
    tkIsKeyword     = \_ -> False
    tkToString      = toString
    tkEquivalent    = tokenCompare


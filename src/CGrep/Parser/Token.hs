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

module CGrep.Parser.Token (parseTokens, filterToken, Token(..), TokenFilter(..), tokenEqual,
                            isIdentifier, isKeyword, isNumber, isBracket, isString, isOperator ) where

import qualified Data.ByteString.Char8 as C
import qualified Data.DList as DL

import Data.Char
    ( isSpace, isAlpha, isAlphaNum, isDigit, isHexDigit )

import Data.Array.Unboxed ( (!), listArray, UArray )

import CGrep.Types ( Text8, Offset )
import Data.List (genericLength)

import CGrep.LanguagesMap ( LanguageInfo (langResKeywords) )
import qualified Data.Set as S

import qualified CGrep.Chunk as T


data TokenState =
    StateSpace        |
    StateIdentifier   |
    StateDigit        |
    StateBracket      |
    StateLit1         |
    StateLit2         |
    StateOther
      deriving (Eq, Enum, Show)


data Token =
    TokenIdentifier  { toString :: !C.ByteString, toOffset :: {-# UNPACK #-} !Offset  } |
    TokenKeyword     { toString :: !C.ByteString, toOffset :: {-# UNPACK #-} !Offset  } |
    TokenDigit       { toString :: !C.ByteString, toOffset :: {-# UNPACK #-} !Offset  } |
    TokenString      { toString :: !C.ByteString, toOffset :: {-# UNPACK #-} !Offset  } |
    TokenBracket     { toString :: !C.ByteString, toOffset :: {-# UNPACK #-} !Offset  } |
    TokenOperator    { toString :: !C.ByteString, toOffset :: {-# UNPACK #-} !Offset  }
       deriving (Show, Eq, Ord)


mkToken :: TokenState -> C.ByteString -> Offset -> Token
mkToken StateSpace      = TokenOperator
mkToken StateIdentifier = TokenIdentifier
mkToken StateDigit      = TokenDigit
mkToken StateBracket    = TokenBracket
mkToken StateLit1       = TokenString
mkToken StateLit2       = TokenString
mkToken StateOther      = TokenOperator


isIdentifier (TokenIdentifier _ _) = True
isIdentifier _  = False
{-# INLINE isIdentifier #-}

isKeyword (TokenKeyword _ _) = True
isKeyword _  = False
{-# INLINE isKeyword #-}

isNumber (TokenDigit _ _) = True
isNumber _  = False
{-# INLINE isNumber #-}

isBracket (TokenBracket _ _) = True
isBracket _  = False
{-# INLINE isBracket #-}

isString (TokenString _ _) = True
isString _  = False
{-# INLINE isString #-}

isOperator (TokenOperator _ _) = True
isOperator _  = False
{-# INLINE isOperator #-}


tokenBuilder :: (C.ByteString -> Offset -> Token) -> Offset -> C.ByteString -> Token
tokenBuilder ctor off ds =  ctor ds (off - fromIntegral (C.length ds))
{-# INLINE tokenBuilder #-}


tokenEqual :: Token -> Token -> Bool
tokenEqual TokenIdentifier  { toString = l } TokenIdentifier { toString = r } = l == r
tokenEqual TokenKeyword     { toString = l } TokenKeyword    { toString = r } = l == r
tokenEqual TokenDigit       { toString = l } TokenDigit      { toString = r } = l == r
tokenEqual TokenString      { toString = l } TokenString     { toString = r } = l == r
tokenEqual TokenBracket     { toString = l } TokenBracket    { toString = r } = l == r
tokenEqual TokenOperator    { toString = l } TokenOperator   { toString = r } = l == r
tokenEqual _ _ = False


data TokenFilter = TokenFilter
    {   filtIdentifier :: !Bool
    ,   filtKeyword    :: !Bool
    ,   filtString     :: !Bool
    ,   filtNumber     :: !Bool
    ,   filtOperator   :: !Bool
    } deriving (Eq)


filterToken :: TokenFilter -> Token -> Bool
filterToken filt TokenIdentifier{} = filtIdentifier filt
filterToken filt TokenKeyword{}    = filtKeyword    filt
filterToken filt TokenDigit{}      = filtNumber     filt
filterToken filt TokenString{}     = filtString     filt
filterToken filt TokenOperator{}   = filtOperator   filt
filterToken filt TokenBracket{}    = False


data TokenAccum = TokenAccum !TokenState {-# UNPACK #-} !Offset {-# UNPACK #-} !Int C.ByteString (DL.DList Token)


parseTokens :: Maybe LanguageInfo -> Text8 -> [Token]
parseTokens linfo xs = fixKeyword linfo <$> (\(TokenAccum ss  off _ acc out) ->
    DL.toList (if C.null acc
                    then out
                    else out `DL.snoc` tokenBuilder (mkToken ss) off acc)) (C.foldl' tokens' (TokenAccum StateSpace 0 0 C.empty DL.empty) xs)
    where tokens' :: TokenAccum -> Char -> TokenAccum
          tokens' (TokenAccum StateSpace off _ _ out) x =
                if | isSpaceLT ! x      ->  TokenAccum StateSpace      (off+1) 0  C.empty         out
                   | x == '\''          ->  TokenAccum StateLit1       (off+1) 0 (C.singleton  x) out
                   | x == '"'           ->  TokenAccum StateLit2       (off+1) 0 (C.singleton  x) out
                   | isAlphaLT ! x      ->  TokenAccum StateIdentifier (off+1) 0 (C.singleton  x) out
                   | isDigitLT ! x      ->  TokenAccum StateDigit      (off+1) 0 (C.singleton  x) out
                   | isBracketLT ! x    ->  TokenAccum StateBracket    (off+1) 0 (C.singleton  x) out
                   | otherwise          ->  TokenAccum StateOther      (off+1) 0 (C.singleton  x) out

          tokens' (TokenAccum StateIdentifier off _ acc out) x =
                if | isAlphaNumLT ! x   ->  TokenAccum StateIdentifier   (off+1) 0 (acc `C.snoc` x)  out
                   | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1) 0  C.empty         (out `DL.snoc` tokenBuilder TokenIdentifier off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenIdentifier off acc)
                   | otherwise          ->  TokenAccum StateOther   (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenIdentifier off acc)

          tokens' (TokenAccum StateDigit off _ acc out) x =
                if | isCharNumberLT ! x ->  TokenAccum StateDigit   (off+1) 0 (acc `C.snoc` x)  out
                   | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1) 0  C.empty         (out `DL.snoc` tokenBuilder TokenDigit off acc)
                   | isAlphaLT ! x      ->  TokenAccum StateIdentifier   (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenDigit off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenDigit off acc)
                   | otherwise          ->  TokenAccum StateOther   (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenDigit off acc)

          tokens' (TokenAccum StateLit1 off skip acc out) x =
                if | skip > 0           ->  TokenAccum StateLit1    (off+1) (skip-1) (acc `C.snoc` x)  out
                   | x == '\\'          ->  TokenAccum StateLit1    (off+1) 1        (acc `C.snoc` x)  out
                   | x == '\''          ->  TokenAccum StateSpace   (off+1) 0         C.empty         (out `DL.snoc` tokenBuilder TokenString (off+1) (acc `C.snoc` '\''))
                   | otherwise          ->  TokenAccum StateLit1    (off+1) 0        (acc `C.snoc` x)  out

          tokens' (TokenAccum StateLit2 off skip acc out) x =
                if | skip > 0           ->  TokenAccum StateLit2    (off+1) (skip-1) (acc `C.snoc` x)  out
                   | x == '\\'          ->  TokenAccum StateLit2    (off+1) 1        (acc `C.snoc` x)  out
                   | x == '"'           ->  TokenAccum StateSpace   (off+1) 0         C.empty         (out `DL.snoc` tokenBuilder TokenString (off+1) (acc `C.snoc` '"'))
                   | otherwise          ->  TokenAccum StateLit2    (off+1) 0        (acc `C.snoc` x)  out

          tokens' (TokenAccum StateBracket off _ acc out) x =
                if | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1) 0  C.empty         (out `DL.snoc` tokenBuilder TokenBracket off acc)
                   | isAlphaLT ! x      ->  TokenAccum StateIdentifier   (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenBracket off acc)
                   | isDigitLT ! x      ->  TokenAccum StateDigit   (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenBracket off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenBracket off acc)
                   | x == '\''          ->  TokenAccum StateLit1    (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenBracket off acc)
                   | x == '"'           ->  TokenAccum StateLit2    (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenBracket off acc)
                   | otherwise          ->  TokenAccum StateOther   (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenBracket off acc)

          tokens' (TokenAccum StateOther off _ acc out) x =
                if | isSpaceLT ! x      ->  TokenAccum StateSpace   (off+1) 0  C.empty         (out `DL.snoc` tokenBuilder TokenOperator off acc)
                   | isAlphaLT ! x      ->  TokenAccum StateIdentifier   (off+1) 0 (C.singleton x)  (out `DL.snoc` tokenBuilder TokenOperator off acc)
                   | isDigitLT ! x      ->  if acc == "."
                                            then TokenAccum StateDigit (off+1) 0 (acc `C.snoc` x)  out
                                            else TokenAccum StateDigit (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenOperator off acc)
                   | isBracketLT ! x    ->  TokenAccum StateBracket    (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenOperator off acc)
                   | x == '\''          ->  TokenAccum StateLit1       (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenBracket off acc)
                   | x == '"'           ->  TokenAccum StateLit2       (off+1) 0 (C.singleton  x) (out `DL.snoc` tokenBuilder TokenBracket off acc)
                   | otherwise          ->  TokenAccum StateOther      (off+1) 0 (acc `C.snoc` x)  out


fixKeyword :: Maybe LanguageInfo -> Token -> Token
fixKeyword (Just linfo) t@(TokenIdentifier s o) =
    if S.member s (langResKeywords linfo)
        then  TokenKeyword s o
         else t
fixKeyword _  t = t


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

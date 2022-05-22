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
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Internal as BI

import qualified Data.DList as DL

import Data.Char ( isSpace, isDigit, isHexDigit, chr )

import CGrep.Types (Offset)
import Data.List (genericLength)

import CGrep.LanguagesMap ( LanguageInfo (langResKeywords, langValidIdentifierChars) )
import qualified Data.Set as S

import qualified CGrep.Chunk as T

import Control.Monad.ST ( ST, runST )
import Data.STRef ( STRef, newSTRef, readSTRef, writeSTRef, modifySTRef', modifySTRef )
import Data.MonoTraversable ( MonoFoldable(oforM_) )
import Data.Word (Word8)

import qualified ByteString.StrictBuilder as B

import CGrep.Parser.Char ( isCharNumber, isBracket', isAlpha_, isAlphaNum_)
import Data.Maybe

data TokenState =
    StateSpace        |
    StateIdentifier   |
    StateDigit        |
    StateBracket      |
    StateLiteral      |
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
mkToken StateLiteral    = TokenString
mkToken StateOther      = TokenOperator


isIdentifier :: Token -> Bool
isIdentifier (TokenIdentifier _ _) = True
isIdentifier _  = False
{-# INLINE isIdentifier #-}

isKeyword :: Token -> Bool
isKeyword (TokenKeyword _ _) = True
isKeyword _  = False
{-# INLINE isKeyword #-}

isNumber :: Token -> Bool
isNumber (TokenDigit _ _) = True
isNumber _  = False
{-# INLINE isNumber #-}

isBracket :: Token -> Bool
isBracket (TokenBracket _ _) = True
isBracket _  = False
{-# INLINE isBracket #-}

isString :: Token -> Bool
isString (TokenString _ _) = True
isString _  = False
{-# INLINE isString #-}

isOperator :: Token -> Bool
isOperator (TokenOperator _ _) = True
isOperator _  = False
{-# INLINE isOperator #-}


tokenBuilder :: (C.ByteString -> Offset -> Token) -> Offset -> B.Builder -> Token
tokenBuilder ctor off b =  ctor ds (off - fromIntegral (B.builderLength b))
    where ds = B.builderBytes b
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


(<~) :: STRef s a -> a -> ST s ()
ref <~ !x = writeSTRef ref x
{-# INLINE (<~) #-}


(~<&>) :: STRef s a -> (a -> a) -> ST s ()
ref ~<&> !x = modifySTRef' ref x
{-# INLINE (~<&>) #-}


parseTokens :: Maybe LanguageInfo -> C.ByteString -> [Token]
parseTokens l t = fixKeyword l <$> runST (parseToken' l t)
  where isAlpha1 = maybe isAlpha_    (fst . langValidIdentifierChars) l
        isAlphaN = maybe isAlphaNum_ (snd . langValidIdentifierChars) l
        parseToken' :: Maybe LanguageInfo -> C.ByteString -> ST a [Token]
        parseToken' linfo txt  = do
          stateR  <- newSTRef StateSpace
          offR    <- newSTRef 0
          accR    <- newSTRef (mempty :: B.Builder)
          tokensR <- newSTRef DL.empty
          oforM_ txt $ \w -> do
            let x = BI.w2c w
            state  <- readSTRef stateR
            off    <- readSTRef offR
            acc    <- readSTRef accR
            case state of
                StateSpace ->
                    if | isSpace  x     -> do stateR <~ StateSpace      ; accR <~ mempty
                       | isAlpha1  x    -> do stateR <~ StateIdentifier ; accR <~ B.asciiChar x
                       | x == chr 2     -> do stateR <~ StateLiteral    ; accR <~ mempty
                       | isDigit  x     -> do stateR <~ StateDigit      ; accR <~ B.asciiChar x
                       | isBracket'  x  -> do stateR <~ StateBracket    ; accR <~ B.asciiChar x
                       | otherwise      -> do stateR <~ StateOther      ; accR <~ B.asciiChar x
                StateIdentifier ->
                    if | isAlphaN   x    ->  do stateR <~ StateIdentifier; accR ~<&> (<> B.asciiChar x)
                       | isSpace  x      ->  do stateR <~ StateSpace     ; accR <~ mempty       ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenIdentifier off acc)
                       | x == chr 2      ->  do stateR <~ StateLiteral   ; accR <~ mempty       ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenIdentifier off acc)
                       | isBracket' x    ->  do stateR <~ StateBracket   ; accR <~ B.asciiChar  x; tokensR ~<&> (`DL.snoc` tokenBuilder TokenIdentifier off acc)
                       | otherwise       ->  do stateR <~ StateOther     ; accR <~ B.asciiChar  x; tokensR ~<&> (`DL.snoc` tokenBuilder TokenIdentifier off acc)

                StateDigit ->
                    if | isCharNumber  x ->  do stateR <~ StateDigit      ; accR ~<&> (<> B.asciiChar x)
                       | isSpace  x      ->  do stateR <~ StateSpace      ; accR <~ mempty         ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenDigit off acc)
                       | x == chr 2      ->  do stateR <~ StateLiteral    ; accR <~ mempty         ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenDigit off acc)
                       | isAlpha1   x    ->  do stateR <~ StateIdentifier ; accR <~ B.asciiChar  x ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenDigit off acc)
                       | isBracket' x    ->  do stateR <~ StateBracket    ; accR <~ B.asciiChar  x ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenDigit off acc)
                       | otherwise       ->  do stateR <~ StateOther      ; accR <~ B.asciiChar  x ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenDigit off acc)

                StateLiteral ->
                    if | x == chr 3      ->  do stateR <~ StateSpace      ; accR <~ mempty; tokensR ~<&> (`DL.snoc` tokenBuilder TokenString off acc)
                       | otherwise       ->  do stateR <~ StateLiteral    ; accR ~<&> (<> B.asciiChar x)

                StateBracket ->
                    if | isSpace  x      ->  do stateR <~ StateSpace      ; accR <~ mempty         ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenBracket off acc)
                       | isAlpha1 x      ->  do stateR <~ StateIdentifier ; accR <~ B.asciiChar  x ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenBracket off acc)
                       | isDigit  x      ->  do stateR <~ StateDigit      ; accR <~ B.asciiChar  x ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenBracket off acc)
                       | isBracket' x    ->  do stateR <~ StateBracket    ; accR <~ B.asciiChar  x ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenBracket off acc)
                       | x == chr 2      ->  do stateR <~ StateLiteral    ; accR <~ mempty         ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenBracket off acc)
                       | otherwise       ->  do stateR <~ StateOther      ; accR <~ B.asciiChar  x ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenBracket off acc)

                StateOther ->
                    if | isSpace  x      ->  do stateR <~ StateSpace      ; accR <~ mempty       ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenOperator off acc)
                       | isAlpha1 x      ->  do stateR <~ StateIdentifier ; accR <~ B.asciiChar x; tokensR ~<&> (`DL.snoc` tokenBuilder TokenOperator off acc)
                       | isDigit  x      ->  if B.builderBytes acc == "."
                                                then do stateR <~ StateDigit ; accR ~<&> (<> B.asciiChar x)
                                                else do stateR <~ StateDigit ; accR <~ B.asciiChar  x; tokensR ~<&> (`DL.snoc` tokenBuilder TokenOperator off acc)
                       | isBracket' x    ->  do stateR <~ StateBracket    ; accR <~ B.asciiChar  x; tokensR ~<&> (`DL.snoc` tokenBuilder TokenOperator off acc)
                       | x == chr 2      ->  do stateR <~ StateLiteral    ; accR <~ mempty        ; tokensR ~<&> (`DL.snoc` tokenBuilder TokenBracket off acc)
                       | otherwise       ->  do stateR <~ StateOther      ; accR ~<&> (<> B.asciiChar x)

            offR ~<&> (+1)

          lastAcc <- readSTRef accR
          tokens  <- readSTRef tokensR

          DL.toList <$>
            if B.builderLength lastAcc == 0
                then return tokens
                else do
                  state   <- readSTRef stateR
                  off     <- readSTRef offR
                  return $ tokens `DL.snoc` tokenBuilder (mkToken state) off lastAcc


fixKeyword :: Maybe LanguageInfo -> Token -> Token
fixKeyword (Just linfo) t@(TokenIdentifier s o) =
    if S.member s (langResKeywords linfo)
        then  TokenKeyword s o
         else t
fixKeyword _  t = t

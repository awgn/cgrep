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

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UnboxedSums #-}

module CGrep.Parser.Token (parseTokens, filterToken, Token(..), TokenFilter(..), tokenEqual,
                            isIdentifier, isKeyword, isNumber, isBracket, isString, isOperator ) where

import qualified Data.ByteString.Char8 as C

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Internal as BI

import qualified Data.DList as DL

import CGrep.Parser.Char
    ( chr,
      isAlphaNum_,
      isAlpha_,
      isBracket',
      isCharNumber,
      isDigit,
      isSpace,
      isCharNumber,
      isBracket',
      isAlpha_,
      isAlphaNum_ )

import CGrep.Types (Offset)
import Data.List (genericLength)

import CGrep.LanguagesMap
    ( CharIdentifierF,
      LanguageInfo(langResKeywords, langIdentifierChars) )
import qualified Data.Set as Set
import qualified Data.Sequence as S
import Data.Sequence ((|>), Seq((:<|), (:|>), Empty))

import qualified CGrep.Chunk as T

import Control.Monad.ST ( ST, runST )
import Data.STRef ( STRef, newSTRef, readSTRef, writeSTRef, modifySTRef', modifySTRef )
import Data.MonoTraversable ( MonoFoldable(oforM_) )
import Data.Word (Word8)

import qualified ByteString.StrictBuilder as B
import CGrep.Chunk

import GHC.Exts ( inline )

data TokenState =
    StateSpace        |
    StateIdentifier   |
    StateDigit        |
    StateBracket      |
    StateLiteral      |
    StateOther
      deriving (Eq, Enum, Show)


data Token =
    TokenIdentifier  { toString :: {-# UNPACK #-}!C.ByteString, toOffset :: {-# UNPACK #-} !Offset  } |
    TokenKeyword     { toString :: {-# UNPACK #-}!C.ByteString, toOffset :: {-# UNPACK #-} !Offset  } |
    TokenDigit       { toString :: {-# UNPACK #-}!C.ByteString, toOffset :: {-# UNPACK #-} !Offset  } |
    TokenString      { toString :: {-# UNPACK #-}!C.ByteString, toOffset :: {-# UNPACK #-} !Offset  } |
    TokenBracket     { toString :: {-# UNPACK #-}!C.ByteString, toOffset :: {-# UNPACK #-} !Offset  } |
    TokenOperator    { toString :: {-# UNPACK #-}!C.ByteString, toOffset :: {-# UNPACK #-} !Offset  }
       deriving (Show, Eq, Ord)


tokenIdentifierOrKeyword :: Maybe LanguageInfo -> C.ByteString -> Offset -> Token
tokenIdentifierOrKeyword Nothing txt off = TokenIdentifier txt off
tokenIdentifierOrKeyword (Just info) txt off =
    if Set.member txt (langResKeywords info)
        then TokenKeyword txt off
        else TokenIdentifier txt off
{-# INLINABLE tokenIdentifierOrKeyword #-}


mkToken :: Maybe LanguageInfo -> TokenState -> C.ByteString -> Offset -> Token
mkToken _ StateSpace         = TokenOperator
mkToken info StateIdentifier = tokenIdentifierOrKeyword info
mkToken _ StateDigit         = TokenDigit
mkToken _ StateBracket       = TokenBracket
mkToken _ StateLiteral       = TokenString
mkToken _ StateOther         = TokenOperator


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
    } deriving (Eq, Show)


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


data TokenIdx = TokenIdx {
    offset :: {-# UNPACK #-}!Int
 ,  len    :: {-# UNPACK #-}!Int
}

tkString :: TokenIdx -> C.ByteString -> C.ByteString
tkString (TokenIdx off len) = C.take len . C.drop off
{-# INLINE tkString #-}


data AccOp = Reset | Start {-# UNPACK #-}!Int | Append {-# UNPACK #-} !Int


(<<~) :: STRef s TokenIdx -> AccOp -> ST s ()
ref <<~ Reset   = writeSTRef ref (TokenIdx (-1) 0)
ref <<~ Start cur = writeSTRef ref (TokenIdx cur 1)
ref <<~ Append cur = modifySTRef' ref $ \case
    TokenIdx (-1) 0  -> TokenIdx cur 1
    TokenIdx off len -> TokenIdx off (len + 1)

{-# INLINE (<<~) #-}


{-# INLINE parseTokens #-}
parseTokens :: Maybe LanguageInfo -> C.ByteString -> S.Seq Token
parseTokens l t = runST (case l >>= langIdentifierChars of
        Nothing                   -> parseToken' isAlpha_ isAlphaNum_ l t
        Just (isAlpha1, isAlphaN) -> parseToken' isAlpha1 isAlphaN l t)
  where parseToken' :: CharIdentifierF -> CharIdentifierF -> Maybe LanguageInfo -> C.ByteString -> ST a (S.Seq Token)
        parseToken' isAlpha1 isAlphaN info txt  = do

          stateR  <- newSTRef StateSpace
          accR    <- newSTRef (TokenIdx (-1) (-1))
          tokensR <- newSTRef S.empty
          curR    <- newSTRef 0

          oforM_ txt $ \w -> do

            let x = BI.w2c w

            cur    <- readSTRef curR
            state  <- readSTRef stateR
            acc    <- readSTRef accR
            tokens <- readSTRef tokensR

            case state of
                StateSpace -> {-# SCC "StateSpace" #-}
                    if | isSpace  x         -> do  accR <<~ Reset
                       | inline isAlpha1  x -> do  stateR <~ StateIdentifier ; accR <<~ Start cur
                       | x == chr 2         -> do  stateR <~ StateLiteral    ; accR <<~ Reset
                       | isDigit  x         -> do  stateR <~ StateDigit      ; accR <<~ Start cur
                       | isBracket'  x      -> do  stateR <~ StateBracket    ; accR <<~ Start cur
                       | otherwise          -> do  stateR <~ StateOther      ; accR <<~ Start cur

                StateIdentifier -> {-# SCC "StateIdentifier" #-}
                    if | inline isAlphaN  x ->  do accR <<~ Append cur
                       | isSpace  x      ->  do stateR <~ StateSpace     ; accR <<~ Reset      ; tokensR <~ (tokens |> buildToken (tokenIdentifierOrKeyword info) acc txt)
                       | x == chr 2      ->  do stateR <~ StateLiteral   ; accR <<~ Reset      ; tokensR <~ (tokens |> buildToken (tokenIdentifierOrKeyword info) acc txt)
                       | isBracket' x    ->  do stateR <~ StateBracket   ; accR <<~ Start cur  ; tokensR <~ (tokens |> buildToken (tokenIdentifierOrKeyword info) acc txt)
                       | otherwise       ->  do stateR <~ StateOther     ; accR <<~ Start cur  ; tokensR <~ (tokens |> buildToken (tokenIdentifierOrKeyword info) acc txt)

                StateDigit -> {-# SCC "StateDigit" #-}
                    if | isCharNumber  x ->  do accR <<~ Append cur
                       | isSpace  x      ->  do stateR <~ StateSpace      ; accR <<~ Reset      ; tokensR <~ (tokens |> buildToken TokenDigit acc txt)
                       | x == chr 2      ->  do stateR <~ StateLiteral    ; accR <<~ Reset      ; tokensR <~ (tokens |> buildToken TokenDigit acc txt)
                       | inline isAlpha1  x    ->  do stateR <~ StateIdentifier ; accR <<~ Start cur  ; tokensR <~ (tokens |> buildToken TokenDigit acc txt)
                       | isBracket' x    ->  do stateR <~ StateBracket    ; accR <<~ Start cur  ; tokensR <~ (tokens |> buildToken TokenDigit acc txt)
                       | otherwise       ->  do stateR <~ StateOther      ; accR <<~ Start cur  ; tokensR <~ (tokens |> buildToken TokenDigit acc txt)

                StateLiteral -> {-# SCC "StateLiteral" #-}
                    if | x == chr 3      ->  do stateR <~ StateSpace      ; accR <<~ Reset; tokensR <~ (tokens |> buildToken TokenString acc txt)
                       | otherwise       ->  do accR <<~ Append cur

                StateBracket -> {-# SCC "StateBracket" #-}
                    if | isSpace  x      ->  do stateR <~ StateSpace      ; accR <<~ Reset          ; tokensR <~ (tokens |> buildToken TokenBracket acc txt)
                       | inline isAlpha1 x      ->  do stateR <~ StateIdentifier ; accR <<~ Start  cur     ; tokensR <~ (tokens |> buildToken TokenBracket acc txt)
                       | isDigit  x      ->  do stateR <~ StateDigit      ; accR <<~ Start  cur     ; tokensR <~ (tokens |> buildToken TokenBracket acc txt)
                       | isBracket' x    ->  do accR <<~ Start  cur     ; tokensR <~ (tokens |> buildToken TokenBracket acc txt)
                       | x == chr 2      ->  do stateR <~ StateLiteral    ; accR <<~ Reset          ; tokensR <~ (tokens |> buildToken TokenBracket acc txt)
                       | otherwise       ->  do stateR <~ StateOther      ; accR <<~ Start cur      ; tokensR <~ (tokens |> buildToken TokenBracket acc txt)

                StateOther -> {-# SCC "StateOther" #-}
                    if | isSpace  x         ->  do stateR <~ StateSpace      ; accR <<~ Reset          ; tokensR <~ (tokens |> buildToken TokenOperator acc txt)
                       | inline isAlpha1 x  ->  do stateR <~ StateIdentifier ; accR <<~ Start cur      ; tokensR <~ (tokens |> buildToken TokenOperator acc txt)
                       | isDigit  x         ->  if tkString acc txt == "."
                                                then do stateR <~ StateDigit ; accR <<~ Append cur
                                                else do stateR <~ StateDigit ; accR <<~ Start cur      ; tokensR <~ (tokens |> buildToken TokenOperator acc txt)
                       | isBracket' x       ->  do stateR <~ StateBracket    ; accR <<~ Append cur     ; tokensR <~ (tokens |> buildToken TokenOperator acc txt)
                       | x == chr 2         ->  do stateR <~ StateLiteral    ; accR <<~ Reset          ; tokensR <~ (tokens |> buildToken TokenBracket  acc txt)
                       | otherwise          ->  do accR <<~ Append cur

            curR <~ (cur + 1)

          lastAcc <- readSTRef accR
          tokens  <- readSTRef tokensR

          if  lastAcc.len == 0
              then return tokens
              else do
                state   <- readSTRef stateR
                cur     <- readSTRef curR
                return $ tokens |> buildToken (mkToken info state) lastAcc txt


buildToken :: (C.ByteString -> Offset -> Token) -> TokenIdx -> C.ByteString -> Token
buildToken f (TokenIdx start len) txt = f (subByteString start len txt) (fromIntegral start)
{-# INLINE buildToken #-}


subByteString :: Int -> Int -> C.ByteString -> C.ByteString
subByteString i n = C.take n . C.drop i
{-# INLINE subByteString #-}

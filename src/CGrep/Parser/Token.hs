{-# LANGUAGE DuplicateRecordFields #-}
--
-- Copyright (c) 2013-2023 Nicola Bonelli <nicola@larthia.com>
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
{-# LANGUAGE OverloadedRecordDot #-}

module CGrep.Parser.Token (
    parseTokens,
    filterToken,
    Token (..),
    TokenFilter (..),
    mkTokenFilter,
    eqToken,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenNumber,
    isTokenBracket,
    isTokenString,
    isTokenOperator,
    isTokenUnspecified,
    tTyp,
    tToken,
    tOffset,
    mkTokenIdentifier,
    mkTokenKeyword,
    mkTokenDigit,
    mkTokenBracket,
    mkTokenString,
    mkTokenOperator,
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as LB
import qualified Data.DList as DL

import CGrep.Parser.Char (
    chr,
    isAlphaNum_,
    isAlpha_,
    isBracket',
    isCharNumber,
    isDigit,
    isPunctuation,
    isSpace,
 )

import CGrep.Types (Offset, Text8)
import Data.List (genericLength)

import CGrep.FileTypeMap (
    CharIdentifierF,
    FileTypeInfo (ftIdentifierChars, ftKeywords),
    WordType (..),
 )

import qualified Data.HashMap.Strict as HM
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (|>))
import qualified Data.Sequence as S

import Control.Monad.ST (ST, runST)
import Data.MonoTraversable (MonoFoldable (oforM_))
import Data.STRef (STRef, modifySTRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Word (Word8)

import qualified ByteString.StrictBuilder as B
import CGrep.Parser.Chunk

import Data.Coerce (coerce)
import Data.Text.Internal.Read (T)
import GHC.Exts (inline)

newtype TokenState = TokenState {unTokenState :: Int}
    deriving newtype (Eq)

instance Show TokenState where
    show StateSpace = "space"
    show StateIdentifier = "identifier"
    show StateDigit = "digit"
    show StateBracket = "bracket"
    show StateLiteral = "literal"
    show StateOther = "other"
    {-# INLINE show #-}

pattern StateSpace :: TokenState
pattern StateSpace = TokenState 0

pattern StateIdentifier :: TokenState
pattern StateIdentifier = TokenState 1

pattern StateDigit :: TokenState
pattern StateDigit = TokenState 2

pattern StateBracket :: TokenState
pattern StateBracket = TokenState 3

pattern StateLiteral :: TokenState
pattern StateLiteral = TokenState 4

pattern StateOther :: TokenState
pattern StateOther = TokenState 5

newtype Token = Token Chunk
    deriving newtype (Eq, Ord)

instance Show Token where
    show (Token (Chunk typ bs off)) = "(" ++ show typ ++ " " ++ C.unpack bs ++ " @" ++ show off ++ ")"
    {-# INLINE show #-}

eqToken :: Token -> Token -> Bool
eqToken a b =
    tToken a == tToken b
        && tTyp a == tTyp b
{-# INLINE eqToken #-}

mkTokenIdentifier :: C.ByteString -> Offset -> Token
mkTokenIdentifier bs off = Token $ Chunk ChunkIdentifier bs off
{-# INLINE mkTokenIdentifier #-}

mkTokenKeyword :: C.ByteString -> Offset -> Token
mkTokenKeyword bs off = Token $ Chunk ChunkKeyword bs off
{-# INLINE mkTokenKeyword #-}

mkTokenDigit :: C.ByteString -> Offset -> Token
mkTokenDigit bs off = Token $ Chunk ChunkDigit bs off
{-# INLINE mkTokenDigit #-}

mkTokenBracket :: C.ByteString -> Offset -> Token
mkTokenBracket bs off = Token $ Chunk ChunkBracket bs off
{-# INLINE mkTokenBracket #-}

mkTokenOperator :: C.ByteString -> Offset -> Token
mkTokenOperator bs off = Token $ Chunk ChunkOperator bs off
{-# INLINE mkTokenOperator #-}

mkTokenString :: C.ByteString -> Offset -> Token
mkTokenString bs off = Token $ Chunk ChunkString bs off
{-# INLINE mkTokenString #-}

mkTokenNativeType :: C.ByteString -> Offset -> Token
mkTokenNativeType bs off = Token $ Chunk ChunkNativeType bs off
{-# INLINE mkTokenNativeType #-}

mkTokenFromWord :: Maybe FileTypeInfo -> C.ByteString -> Offset -> Token
mkTokenFromWord Nothing txt off = mkTokenIdentifier txt off
mkTokenFromWord (Just info) txt off =
    case HM.lookup txt (ftKeywords info) of
        Just typ -> case typ of
            Keyword -> mkTokenKeyword txt off
            NativeType -> mkTokenNativeType txt off
        _ -> mkTokenIdentifier txt off
{-# INLINEABLE mkTokenFromWord #-}

mkToken :: Maybe FileTypeInfo -> TokenState -> C.ByteString -> Offset -> Token
mkToken _ StateSpace = mkTokenOperator
mkToken info StateIdentifier = mkTokenFromWord info
mkToken _ StateDigit = mkTokenDigit
mkToken _ StateBracket = mkTokenBracket
mkToken _ StateLiteral = mkTokenString
mkToken _ StateOther = mkTokenOperator

tTyp :: Token -> ChunkType
tTyp = cTyp . coerce
{-# INLINE tTyp #-}

tOffset :: Token -> Offset
tOffset t = cOffset (coerce t :: Chunk)
{-# INLINE tOffset #-}

tToken :: Token -> Text8
tToken t = cToken (coerce t :: Chunk)
{-# INLINE tToken #-}

isTokenIdentifier :: Token -> Bool
isTokenIdentifier t = cTyp (coerce t) == ChunkIdentifier
{-# INLINE isTokenIdentifier #-}

isTokenKeyword :: Token -> Bool
isTokenKeyword t = cTyp (coerce t) == ChunkKeyword
{-# INLINE isTokenKeyword #-}

isTokenNumber :: Token -> Bool
isTokenNumber t = cTyp (coerce t) == ChunkDigit
{-# INLINE isTokenNumber #-}

isTokenBracket :: Token -> Bool
isTokenBracket t = cTyp (coerce t) == ChunkBracket
{-# INLINE isTokenBracket #-}

isTokenOperator :: Token -> Bool
isTokenOperator t = cTyp (coerce t) == ChunkOperator
{-# INLINE isTokenOperator #-}

isTokenString :: Token -> Bool
isTokenString t = cTyp (coerce t) == ChunkString
{-# INLINE isTokenString #-}

isTokenNativeType :: Token -> Bool
isTokenNativeType t = cTyp (coerce t) == ChunkNativeType
{-# INLINE isTokenNativeType #-}

isTokenUnspecified :: Token -> Bool
isTokenUnspecified t = cTyp (coerce t) == ChunkUnspec
{-# INLINE isTokenUnspecified #-}

data TokenFilter = TokenFilter
    { tfIdentifier :: !Bool
    , tfKeyword :: !Bool
    , tfNativeType :: !Bool
    , tfString :: !Bool
    , tfNumber :: !Bool
    , tfOperator :: !Bool
    , tfBracket :: !Bool
    }
    deriving stock (Eq, Show)

filterToken :: TokenFilter -> Token -> Bool
filterToken f t = case cTyp (coerce t :: Chunk) of
    ChunkIdentifier -> tfIdentifier f
    ChunkKeyword -> tfKeyword f
    ChunkDigit -> tfNumber f
    ChunkOperator -> tfOperator f
    ChunkString -> tfString f
    ChunkNativeType -> tfNativeType f
    ChunkBracket -> tfBracket f
    ChunkUnspec -> False

mkTokenFilter :: (Traversable t) => t ChunkType -> TokenFilter
mkTokenFilter = foldr go (TokenFilter False False False False False False False)
  where
    go ChunkIdentifier f = f{tfIdentifier = True}
    go ChunkKeyword f = f{tfKeyword = True}
    go ChunkNativeType f = f{tfNativeType = True}
    go ChunkDigit f = f{tfNumber = True}
    go ChunkOperator f = f{tfOperator = True}
    go ChunkString f = f{tfString = True}
    go ChunkBracket f = f{tfBracket = True}
    go ChunkUnspec f = f

(<~) :: STRef s a -> a -> ST s ()
ref <~ !x = writeSTRef ref x
{-# INLINE (<~) #-}

data TokenIdx = TokenIdx
    { offset :: {-# UNPACK #-} !Int
    , len :: {-# UNPACK #-} !Int
    }

tkString :: TokenIdx -> C.ByteString -> C.ByteString
tkString (TokenIdx off len) = C.take len . C.drop off
{-# INLINE tkString #-}

data AccOp = Reset | Start {-# UNPACK #-} !Int | Append {-# UNPACK #-} !Int

(<<~) :: STRef s TokenIdx -> AccOp -> ST s ()
ref <<~ Reset = writeSTRef ref (TokenIdx (-1) 0)
ref <<~ Start cur = writeSTRef ref (TokenIdx cur 1)
ref <<~ Append cur = modifySTRef' ref $ \case
    TokenIdx (-1) 0 -> TokenIdx cur 1
    TokenIdx off len -> TokenIdx off (len + 1)
{-# INLINE (<<~) #-}

{-# INLINE parseTokens #-}
parseTokens :: TokenFilter -> Maybe FileTypeInfo -> C.ByteString -> S.Seq Token
parseTokens f@TokenFilter{..} l t =
    runST
        ( case l >>= ftIdentifierChars of
            Nothing -> parseToken' isAlpha_ isAlphaNum_ l t
            Just (isAlpha1, isAlphaN) -> parseToken' isAlpha1 isAlphaN l t
        )
  where
    parseToken' :: CharIdentifierF -> CharIdentifierF -> Maybe FileTypeInfo -> C.ByteString -> ST a (S.Seq Token)
    parseToken' isAlpha1 isAlphaN info txt = do
        stateR <- newSTRef StateSpace
        accR <- newSTRef (TokenIdx (-1) (-1))
        tokensR <- newSTRef S.empty
        curR <- newSTRef 0

        oforM_ txt $ \w -> do
            let x = BI.w2c w
            cur <- readSTRef curR
            state <- readSTRef stateR

            case state of
                StateSpace ->
                    {-# SCC "StateSpace" #-}
                    if
                        | isSpace x -> do accR <<~ Reset
                        | inline isAlpha1 x -> do stateR <~ StateIdentifier; accR <<~ Start cur
                        | x == chr 2 -> do stateR <~ StateLiteral; accR <<~ Reset
                        | isDigit x -> do stateR <~ StateDigit; accR <<~ Start cur
                        | isBracket' x -> do stateR <~ StateBracket; accR <<~ Start cur
                        | otherwise -> do stateR <~ StateOther; accR <<~ Start cur
                StateIdentifier ->
                    {-# SCC "StateIdentifier" #-}
                    if isAlphaN x
                        then accR <<~ Append cur
                        else do
                            acc <- readSTRef accR
                            tokens <- readSTRef tokensR
                            if
                                | isSpace x -> do stateR <~ StateSpace; accR <<~ Reset; tokensR <~ (tokens |> buildToken_ tfIdentifier tfKeyword tfNativeType (mkTokenFromWord info) acc txt)
                                | x == chr 2 -> do stateR <~ StateLiteral; accR <<~ Reset; tokensR <~ (tokens |> buildToken_ tfIdentifier tfKeyword tfNativeType (mkTokenFromWord info) acc txt)
                                | isBracket' x -> do stateR <~ StateBracket; accR <<~ Start cur; tokensR <~ (tokens |> buildToken_ tfIdentifier tfKeyword tfNativeType (mkTokenFromWord info) acc txt)
                                | otherwise -> do stateR <~ StateOther; accR <<~ Start cur; tokensR <~ (tokens |> buildToken_ tfIdentifier tfKeyword tfNativeType (mkTokenFromWord info) acc txt)
                StateDigit ->
                    {-# SCC "StateDigit" #-}
                    if isCharNumber x
                        then accR <<~ Append cur
                        else do
                            acc <- readSTRef accR
                            tokens <- readSTRef tokensR
                            if
                                | isSpace x -> do stateR <~ StateSpace; accR <<~ Reset; tokensR <~ (tokens |> buildToken tfNumber mkTokenDigit acc txt)
                                | x == chr 2 -> do stateR <~ StateLiteral; accR <<~ Reset; tokensR <~ (tokens |> buildToken tfNumber mkTokenDigit acc txt)
                                | inline isAlpha1 x -> do stateR <~ StateIdentifier; accR <<~ Start cur; tokensR <~ (tokens |> buildToken tfNumber mkTokenDigit acc txt)
                                | isBracket' x -> do stateR <~ StateBracket; accR <<~ Start cur; tokensR <~ (tokens |> buildToken tfNumber mkTokenDigit acc txt)
                                | otherwise -> do stateR <~ StateOther; accR <<~ Start cur; tokensR <~ (tokens |> buildToken tfNumber mkTokenDigit acc txt)
                StateLiteral ->
                    {-# SCC "StateLiteral" #-}
                    if x == chr 3
                        then do
                            acc <- readSTRef accR
                            tokens <- readSTRef tokensR
                            stateR <~ StateSpace
                            accR <<~ Reset
                            tokensR <~ (tokens |> buildToken tfString mkTokenString acc txt)
                        else do accR <<~ Append cur
                StateBracket ->
                    {-# SCC "StateBracket" #-}
                    do
                        acc <- readSTRef accR
                        tokens <- readSTRef tokensR
                        if
                            | isSpace x -> do stateR <~ StateSpace; accR <<~ Reset; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
                            | inline isAlpha1 x -> do stateR <~ StateIdentifier; accR <<~ Start cur; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
                            | isDigit x -> do stateR <~ StateDigit; accR <<~ Start cur; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
                            | isBracket' x -> do accR <<~ Start cur; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
                            | x == chr 2 -> do stateR <~ StateLiteral; accR <<~ Reset; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
                            | otherwise -> do stateR <~ StateOther; accR <<~ Start cur; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
                StateOther ->
                    {-# SCC "StateOther" #-}
                    do
                        acc <- readSTRef accR
                        tokens <- readSTRef tokensR
                        if
                            | isSpace x -> do stateR <~ StateSpace; accR <<~ Reset; tokensR <~ (tokens |> buildToken tfOperator mkTokenOperator acc txt)
                            | inline isAlpha1 x -> do stateR <~ StateIdentifier; accR <<~ Start cur; tokensR <~ (tokens |> buildToken tfOperator mkTokenOperator acc txt)
                            | isDigit x ->
                                if tkString acc txt == "."
                                    then do stateR <~ StateDigit; accR <<~ Append cur
                                    else do stateR <~ StateDigit; accR <<~ Start cur; tokensR <~ (tokens |> buildToken tfOperator mkTokenOperator acc txt)
                            | isBracket' x -> do stateR <~ StateBracket; accR <<~ Append cur; tokensR <~ (tokens |> buildToken tfOperator mkTokenOperator acc txt)
                            | x == chr 2 -> do stateR <~ StateLiteral; accR <<~ Reset; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
                            | isPunctuation x -> do accR <<~ Start cur; tokensR <~ (tokens |> buildToken tfOperator mkTokenOperator acc txt)
                            | otherwise -> do accR <<~ Append cur

            curR <~ (cur + 1)

        lastAcc <- readSTRef accR
        tokens <- readSTRef tokensR

        if lastAcc.len == 0
            then return tokens
            else do
                state <- readSTRef stateR
                cur <- readSTRef curR
                return $ tokens |> buildFilteredToken f (mkToken info state) lastAcc txt

buildFilteredToken :: TokenFilter -> (C.ByteString -> Offset -> Token) -> TokenIdx -> C.ByteString -> Token
buildFilteredToken tf f (TokenIdx start len) txt =
    let t = f (subByteString start len txt) (fromIntegral start)
     in if filterToken tf t
            then t
            else unspecifiedToken
{-# INLINE buildFilteredToken #-}

buildToken :: Bool -> (C.ByteString -> Offset -> Token) -> TokenIdx -> C.ByteString -> Token
buildToken True f (TokenIdx start len) txt = f (subByteString start len txt) (fromIntegral start)
buildToken False f (TokenIdx start len) txt = unspecifiedToken
{-# INLINE buildToken #-}

buildToken_ :: Bool -> Bool -> Bool -> (C.ByteString -> Offset -> Token) -> TokenIdx -> C.ByteString -> Token
buildToken_ i k t f (TokenIdx start len) txt =
    if i && isTokenIdentifier tok || k && isTokenKeyword tok || t && isTokenNativeType tok
        then tok
        else unspecifiedToken
  where
    tok = f (subByteString start len txt) (fromIntegral start)

subByteString :: Int -> Int -> C.ByteString -> C.ByteString
subByteString i n = C.take n . C.drop i
{-# INLINE subByteString #-}

unspecifiedToken :: Token
unspecifiedToken = Token $ Chunk ChunkUnspec C.empty 0

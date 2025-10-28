{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
--
-- Copyright (c) 2013-2025 Nicola Bonelli <nicola@larthia.com>
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

-- import qualified Data.DList as DL

-- import CGrep.Parser.Char (
--     chr,
--     isAlphaNum_,
--     isAlpha_,
--     isBracket',
--     isCharNumber,
--     isDigit,
--     isPunctuation,
--     isSpace,
--  )

-- import Data.List (genericLength)

import CGrep.FileTypeMap (
    -- CharIdentifierF,
    CharSet (..),
    FileTypeInfo (..),
    IsCharSet (..),
    WordType (..),
 )

import qualified Data.HashMap.Strict as HM

-- import Data.Sequence (Seq (Empty, (:<|), (:|>)), (|>))
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S

import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)

import CGrep.Parser.Chunk

import CGrep.ContextFilter
import CGrep.Parser.Char (isBracket', isCharNumber, isDigit, isPunctuation, isSpace)
import CGrep.Text (iterM, textOffsetWord8, textSlice)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import GHC.Exts (inline)

newtype TokenState = TokenState {_unTokenState :: Int}
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

{-# COMPLETE StateSpace, StateIdentifier, StateDigit, StateBracket, StateLiteral, StateOther #-}

newtype Token = Token Chunk
    deriving newtype (Eq, Ord)

instance Show Token where
    show (Token (Chunk typ txt)) = case typ of
        ChunkUnspec -> "(*)"
        _ -> "(" <> show typ <> " '" <> T.unpack txt <> "' @" <> show (textOffsetWord8 txt) <> ")"
    {-# INLINE show #-}

eqToken :: Token -> Token -> Bool
eqToken a b =
    tToken a == tToken b
        && tTyp a == tTyp b
{-# INLINE eqToken #-}

mkTokenIdentifier :: T.Text -> Token
mkTokenIdentifier bs = Token $ Chunk ChunkIdentifier bs
{-# INLINE mkTokenIdentifier #-}

mkTokenKeyword :: T.Text -> Token
mkTokenKeyword bs = Token $ Chunk ChunkKeyword bs
{-# INLINE mkTokenKeyword #-}

mkTokenDigit :: T.Text -> Token
mkTokenDigit bs = Token $ Chunk ChunkDigit bs
{-# INLINE mkTokenDigit #-}

mkTokenBracket :: T.Text -> Token
mkTokenBracket bs = Token $ Chunk ChunkBracket bs
{-# INLINE mkTokenBracket #-}

mkTokenOperator :: T.Text -> Token
mkTokenOperator bs = Token $ Chunk ChunkOperator bs
{-# INLINE mkTokenOperator #-}

mkTokenString :: T.Text -> Token
mkTokenString bs = Token $ Chunk ChunkString bs
{-# INLINE mkTokenString #-}

mkTokenNativeType :: T.Text -> Token
mkTokenNativeType bs = Token $ Chunk ChunkNativeType bs
{-# INLINE mkTokenNativeType #-}

mkTokenFromWord :: Maybe FileTypeInfo -> T.Text -> Token
mkTokenFromWord Nothing txt = mkTokenIdentifier txt
mkTokenFromWord (Just info) txt =
    case HM.lookup txt (ftKeywords info) of
        Just typ -> case typ of
            Keyword -> mkTokenKeyword txt
            NativeType -> mkTokenNativeType txt
        _ -> mkTokenIdentifier txt
{-# INLINEABLE mkTokenFromWord #-}

mkToken :: Maybe FileTypeInfo -> TokenState -> T.Text -> Token
mkToken info state = case state of
    StateSpace -> mkTokenOperator
    StateIdentifier -> mkTokenFromWord info
    StateDigit -> mkTokenDigit
    StateBracket -> mkTokenBracket
    StateLiteral -> mkTokenString
    StateOther -> mkTokenOperator
{-# INLINE mkToken #-}

tTyp :: Token -> ChunkType
tTyp = cTyp . coerce
{-# INLINE tTyp #-}

tOffset :: Token -> Int
tOffset t = cOffset (coerce t :: Chunk)
{-# INLINE tOffset #-}

tToken :: Token -> T.Text
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
    { tfIdentifier :: {-# UNPACK #-} !Bool
    , tfKeyword :: {-# UNPACK #-} !Bool
    , tfNativeType :: {-# UNPACK #-} !Bool
    , tfString :: {-# UNPACK #-} !Bool
    , tfNumber :: {-# UNPACK #-} !Bool
    , tfOperator :: {-# UNPACK #-} !Bool
    , tfBracket :: {-# UNPACK #-} !Bool
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
    { _offset :: {-# UNPACK #-} !Int
    , len :: {-# UNPACK #-} !Int
    }

tkString :: TokenIdx -> T.Text -> T.Text
tkString (TokenIdx off len) = TU.takeWord8 len . TU.dropWord8 off
{-# INLINE tkString #-}

data AccOp = Reset | Start {-# UNPACK #-} !Int !Int | Append {-# UNPACK #-} !Int !Int

(<<~) :: STRef s TokenIdx -> AccOp -> ST s ()
ref <<~ Reset = writeSTRef ref (TokenIdx (-1) 0)
ref <<~ Start offset delta = writeSTRef ref (TokenIdx offset delta)
ref <<~ Append offset delta = modifySTRef' ref $ \case
    TokenIdx (-1) 0 -> TokenIdx offset delta
    TokenIdx offset' delta' -> TokenIdx offset' (delta + delta')
{-# INLINE (<<~) #-}

{-# INLINE parseTokens #-}
parseTokens :: TokenFilter -> Maybe FileTypeInfo -> Bool -> T.Text -> Seq Token
parseTokens tf info strict txt =
    runST $ do
        let (runtimeCS1, runtimeCS2) = fromMaybe (None, None) (info >>= ftIdentCharSet)
        case (runtimeCS1, runtimeCS2) of
            (None, None) -> parseToken' @'None @'None tf info True txt
            (Alpha_, AlphaNum_') -> parseToken' @'Alpha_ @'AlphaNum_' tf info strict txt
            (Alpha_, AlphaNum_) -> parseToken' @'Alpha_ @'AlphaNum_ tf info strict txt
            (Alpha, AlphaNum_) -> parseToken' @'Alpha @'AlphaNum_ tf info strict txt
            (Alpha_, AlphaNumDash_) -> parseToken' @'Alpha_ @'AlphaNumDash_ tf info strict txt
            (AlphaDash_, AlphaNumDash_) -> parseToken' @'AlphaDash_ @'AlphaNumDash_ tf info strict txt
            (ClojureIdentStart, ClojureIdentCont) -> parseToken' @'ClojureIdentStart @'ClojureIdentCont tf info strict txt
            (CSharpIdentStart, CSharpIdentCont) -> parseToken' @'CSharpIdentStart @'CSharpIdentCont tf info strict txt
            (HtmlIdentStart, HtmlIdentCont) -> parseToken' @'HtmlIdentStart @'HtmlIdentCont tf info strict txt
            (JavaIdentStart, JavaIdentCont) -> parseToken' @'JavaIdentStart @'JavaIdentCont tf info strict txt
            (JuliaIdentStart, JuliaIdentCont) -> parseToken' @'JuliaIdentStart @'JuliaIdentCont tf info strict txt
            (ListIdent, ListIdent) -> parseToken' @'ListIdent @'ListIdent tf info strict txt
            (Unicode_, UnicodeNum_) -> parseToken' @'Unicode_ @'UnicodeNum_ tf info strict txt
            (UnicodeDollar_, UnicodeNumDollar_) -> parseToken' @'UnicodeDollar_ @'UnicodeNumDollar_ tf info strict txt
            (Unicode_, UnicodeNum_') -> parseToken' @'Unicode_ @'UnicodeNum_' tf info strict txt
            (Unicode_, UnicodeNumDollar_) -> parseToken' @'Unicode_ @'UnicodeNumDollar_ tf info strict txt
            (UnicodeXIDStart_, UnicodeNumXIDCont_) -> parseToken' @'UnicodeXIDStart_ @'UnicodeNumXIDCont_ tf info strict txt
            (AgdaIdent, AgdaIdent) -> parseToken' @'AgdaIdent @'AgdaIdent tf info strict txt
            _ -> error $ "CGrep: unsupported CharSet combination: " <> show (runtimeCS1, runtimeCS2)

parseToken' :: forall (cs1 :: CharSet) (cs :: CharSet) a. (IsCharSet cs1, IsCharSet cs) => TokenFilter -> Maybe FileTypeInfo -> Bool -> T.Text -> ST a (S.Seq Token)
parseToken' tf@TokenFilter{..} info strict txt = do
    stateR <- newSTRef StateSpace
    accR <- newSTRef (TokenIdx (-1) (-1))
    tokensR <- newSTRef S.empty

    iterM txt $ \(# x, cur, delta #) -> do
        state <- readSTRef stateR
        case state of
            StateSpace ->
                if
                    | isSpace x -> do accR <<~ Reset
                    | inline (isValidChar @cs1 x) -> do stateR <~ StateIdentifier; accR <<~ Start cur delta
                    | x == startLiteralMarker -> do stateR <~ StateLiteral; accR <<~ Reset
                    | isDigit x -> do stateR <~ StateDigit; accR <<~ Start cur delta
                    | isBracket' x -> do stateR <~ StateBracket; accR <<~ Start cur delta
                    | otherwise -> do stateR <~ StateOther; accR <<~ Start cur delta
            StateIdentifier ->
                if isValidChar @cs x
                    then accR <<~ Append cur delta
                    else do
                        acc <- readSTRef accR
                        tokens <- readSTRef tokensR
                        if
                            | isSpace x -> do stateR <~ StateSpace; accR <<~ Reset; tokensR <~ (tokens |> buildTokenIf tfIdentifier tfKeyword tfNativeType (mkTokenFromWord info) acc txt)
                            | x == startLiteralMarker -> do stateR <~ StateLiteral; accR <<~ Reset; tokensR <~ (tokens |> buildTokenIf tfIdentifier tfKeyword tfNativeType (mkTokenFromWord info) acc txt)
                            | isBracket' x -> do stateR <~ StateBracket; accR <<~ Start cur delta; tokensR <~ (tokens |> buildTokenIf tfIdentifier tfKeyword tfNativeType (mkTokenFromWord info) acc txt)
                            | otherwise -> do stateR <~ StateOther; accR <<~ Start cur delta; tokensR <~ (tokens |> buildTokenIf tfIdentifier tfKeyword tfNativeType (mkTokenFromWord info) acc txt)
            StateDigit ->
                if isCharNumber x
                    then accR <<~ Append cur delta
                    else do
                        acc <- readSTRef accR
                        tokens <- readSTRef tokensR
                        if
                            | isSpace x -> do stateR <~ StateSpace; accR <<~ Reset; tokensR <~ (tokens |> buildToken tfNumber mkTokenDigit acc txt)
                            | x == startLiteralMarker -> do stateR <~ StateLiteral; accR <<~ Reset; tokensR <~ (tokens |> buildToken tfNumber mkTokenDigit acc txt)
                            | inline (isValidChar @cs1 x) -> do stateR <~ StateIdentifier; accR <<~ Start cur delta; tokensR <~ (tokens |> buildToken tfNumber mkTokenDigit acc txt)
                            | isBracket' x -> do stateR <~ StateBracket; accR <<~ Start cur delta; tokensR <~ (tokens |> buildToken tfNumber mkTokenDigit acc txt)
                            | otherwise -> do stateR <~ StateOther; accR <<~ Start cur delta; tokensR <~ (tokens |> buildToken tfNumber mkTokenDigit acc txt)
            StateLiteral ->
                if x == endLiteralMarker
                    then do
                        acc <- readSTRef accR
                        tokens <- readSTRef tokensR
                        stateR <~ StateSpace
                        accR <<~ Reset
                        tokensR <~ (tokens |> buildToken tfString mkTokenString acc txt)
                    else do accR <<~ Append cur delta
            StateBracket ->
                do
                    acc <- readSTRef accR
                    tokens <- readSTRef tokensR
                    if
                        | isSpace x -> do stateR <~ StateSpace; accR <<~ Reset; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
                        | inline (isValidChar @cs1 x) -> do stateR <~ StateIdentifier; accR <<~ Start cur delta; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
                        | isDigit x -> do stateR <~ StateDigit; accR <<~ Start cur delta; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
                        | isBracket' x -> do accR <<~ Start cur delta; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
                        | x == startLiteralMarker -> do stateR <~ StateLiteral; accR <<~ Reset; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
                        | otherwise -> do stateR <~ StateOther; accR <<~ Start cur delta; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
            StateOther ->
                do
                    acc <- readSTRef accR
                    tokens <- readSTRef tokensR
                    if
                        | isSpace x -> do stateR <~ StateSpace; accR <<~ Reset; tokensR <~ (tokens |> buildToken tfOperator mkTokenOperator acc txt)
                        | inline (isValidChar @cs1 x) -> do stateR <~ StateIdentifier; accR <<~ Start cur delta; tokensR <~ (tokens |> buildToken tfOperator mkTokenOperator acc txt)
                        | isDigit x ->
                            if tkString acc txt == "."
                                then do stateR <~ StateDigit; accR <<~ Append cur delta
                                else do stateR <~ StateDigit; accR <<~ Start cur delta; tokensR <~ (tokens |> buildToken tfOperator mkTokenOperator acc txt)
                        | isBracket' x -> do stateR <~ StateBracket; accR <<~ Start cur delta; tokensR <~ (tokens |> buildToken tfOperator mkTokenOperator acc txt)
                        | x == startLiteralMarker -> do stateR <~ StateLiteral; accR <<~ Reset; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
                        | isPunctuation x -> do accR <<~ Start cur delta; tokensR <~ (tokens |> buildToken tfOperator mkTokenOperator acc txt)
                        | otherwise ->
                            if strict
                                then do accR <<~ Append cur delta
                                else do accR <<~ Start cur delta; tokensR <~ (tokens |> buildToken tfOperator mkTokenOperator acc txt)

    lastAcc <- readSTRef accR
    tokens <- readSTRef tokensR
    if lastAcc.len == 0
        then return tokens
        else do
            state <- readSTRef stateR
            return $ tokens |> buildFilteredToken tf (mkToken info state) lastAcc txt

buildFilteredToken :: TokenFilter -> (T.Text -> Token) -> TokenIdx -> T.Text -> Token
buildFilteredToken tf f (TokenIdx start len) txt =
    let t = f (textSlice txt start len)
     in if filterToken tf t
            then t
            else unspecifiedToken
{-# INLINE buildFilteredToken #-}

buildToken :: Bool -> (T.Text -> Token) -> TokenIdx -> T.Text -> Token
buildToken True f (TokenIdx start len) txt = f (textSlice txt start len)
buildToken False _ (TokenIdx _start _len) _txt = unspecifiedToken
{-# INLINE buildToken #-}

buildTokenIf :: Bool -> Bool -> Bool -> (T.Text -> Token) -> TokenIdx -> T.Text -> Token
buildTokenIf i k t f (TokenIdx start len) txt =
    if i && isTokenIdentifier tok || k && isTokenKeyword tok || t && isTokenNativeType tok
        then tok
        else unspecifiedToken
  where
    tok = f (textSlice txt start len)

unspecifiedToken :: Token
unspecifiedToken = Token $ Chunk ChunkUnspec T.empty
{-# INLINE unspecifiedToken #-}

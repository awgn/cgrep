{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module CGrep.Parser.Token (
    parseTokens,
    parseTokensRef,
    filterToken,
    Token (..),
    TokenFilter (..),
    mkTokenFilter,
    eqToken,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenNativeType,
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
    mkTokenNativeType,
    mkTokenDigit,
    mkTokenBracket,
    mkTokenString,
    mkTokenOperator,
) where

import CGrep.FileTypeMap (
    CharSet (..),
    FileTypeInfo (..),
    IsCharSet (..),
    WordType (..),
 )
import CGrep.Parser.Char (isBracket', isCharNumber, isDigit, isPunctuation, isSpace)
import CGrep.Parser.Chunk (
    Chunk (..),
    ChunkType,
    cOffset,
    pattern ChunkBracket,
    pattern ChunkDigit,
    pattern ChunkIdentifier,
    pattern ChunkKeyword,
    pattern ChunkNativeType,
    pattern ChunkOperator,
    pattern ChunkString,
    pattern ChunkUnspec,
 )
import CGrep.Semantic.ContextFilter (endLiteralMarker, startLiteralMarker)
import CGrep.Text (iterM, textOffsetWord8, textSlice)
import Control.Monad.ST (ST, runST)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Sequence ((|>))
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import Data.Text.Internal.ArrayUtils (memchr)
import qualified Data.Text.Unsafe as TU
import Data.Word (Word8)
import GHC.Exts (Char (C#), Int (I#), chr#, inline)

endLiteralMarkerByte :: Word8
endLiteralMarkerByte = 3

unsafeChr8 :: Word8 -> Char
unsafeChr8 !w =
    let !(I# i#) = fromIntegral w
     in C# (chr# i#)
{-# INLINE unsafeChr8 #-}

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

{-# INLINE parseTokens #-}
parseTokens :: TokenFilter -> Maybe FileTypeInfo -> Bool -> T.Text -> [Token]
parseTokens tf info strict txt =
    let (runtimeCS1, runtimeCS2) = fromMaybe (None, None) (info >>= ftIdentCharSet)
     in case (runtimeCS1, runtimeCS2) of
            (None, None) -> parseTokensFast @'None @'None tf info True txt
            (Alpha_, AlphaNum_') -> parseTokensFast @'Alpha_ @'AlphaNum_' tf info strict txt
            (Alpha_, AlphaNum_) -> parseTokensFast @'Alpha_ @'AlphaNum_ tf info strict txt
            (Alpha, AlphaNum_) -> parseTokensFast @'Alpha @'AlphaNum_ tf info strict txt
            (Alpha_, AlphaNumDash_) -> parseTokensFast @'Alpha_ @'AlphaNumDash_ tf info strict txt
            (AlphaDash_, AlphaNumDash_) -> parseTokensFast @'AlphaDash_ @'AlphaNumDash_ tf info strict txt
            (ClojureIdentStart, ClojureIdentCont) -> parseTokensFast @'ClojureIdentStart @'ClojureIdentCont tf info strict txt
            (CSharpIdentStart, CSharpIdentCont) -> parseTokensFast @'CSharpIdentStart @'CSharpIdentCont tf info strict txt
            (HtmlIdentStart, HtmlIdentCont) -> parseTokensFast @'HtmlIdentStart @'HtmlIdentCont tf info strict txt
            (JavaIdentStart, JavaIdentCont) -> parseTokensFast @'JavaIdentStart @'JavaIdentCont tf info strict txt
            (JuliaIdentStart, JuliaIdentCont) -> parseTokensFast @'JuliaIdentStart @'JuliaIdentCont tf info strict txt
            (ListIdent, ListIdent) -> parseTokensFast @'ListIdent @'ListIdent tf info strict txt
            (Unicode_, UnicodeNum_) -> parseTokensFast @'Unicode_ @'UnicodeNum_ tf info strict txt
            (UnicodeDollar_, UnicodeNumDollar_) -> parseTokensFast @'UnicodeDollar_ @'UnicodeNumDollar_ tf info strict txt
            (Unicode_, UnicodeNum_') -> parseTokensFast @'Unicode_ @'UnicodeNum_' tf info strict txt
            (Unicode_, UnicodeNumDollar_) -> parseTokensFast @'Unicode_ @'UnicodeNumDollar_ tf info strict txt
            (UnicodeXIDStart_, UnicodeNumXIDCont_) -> parseTokensFast @'UnicodeXIDStart_ @'UnicodeNumXIDCont_ tf info strict txt
            (AgdaIdent, AgdaIdent) -> parseTokensFast @'AgdaIdent @'AgdaIdent tf info strict txt
            _ -> error $ "CGrep: unsupported CharSet combination: " <> show (runtimeCS1, runtimeCS2)

parseTokensFast :: forall (cs1 :: CharSet) (cs :: CharSet). (IsCharSet cs1, IsCharSet cs) => TokenFilter -> Maybe FileTypeInfo -> Bool -> T.Text -> [Token]
parseTokensFast TokenFilter{..} info !strict txt@(TI.Text arr@(A.ByteArray arr#) srcOff byteLen)
    | byteLen <= 0 = []
    | otherwise = goSpace srcOff []
  where
    !endOff = srcOff + byteLen
    !kwMap = maybe HM.empty ftKeywords info
    !hasKw = not (HM.null kwMap)
    !wantAnyWord = tfIdentifier || tfKeyword || tfNativeType

    {-# INLINE decodeAt #-}
    decodeAt :: Int -> (# Char, Int #)
    decodeAt !ix =
        let !b0 = A.unsafeIndex arr ix
         in if b0 < 0x80
                then (# unsafeChr8 b0, 1 #)
                else
                    let TU.Iter !c !d = TU.iter txt (ix - srcOff)
                     in (# c, d #)

    {-# INLINE pushWord #-}
    pushWord :: Int -> Int -> [Token] -> [Token]
    pushWord !beg !end !acc
        | not wantAnyWord = acc
        | not hasKw =
            if tfIdentifier
                then
                    let !tok = mkTokenIdentifier (TI.Text arr beg (end - beg))
                     in tok : acc
                else acc
        | otherwise =
            let !slice = TI.Text arr beg (end - beg)
             in case HM.lookup slice kwMap of
                    Nothing ->
                        if tfIdentifier
                            then let !tok = mkTokenIdentifier slice in tok : acc
                            else acc
                    Just Keyword ->
                        if tfKeyword
                            then let !tok = mkTokenKeyword slice in tok : acc
                            else acc
                    Just NativeType ->
                        if tfNativeType
                            then let !tok = mkTokenNativeType slice in tok : acc
                            else acc

    {-# INLINE pushDigit #-}
    pushDigit :: Int -> Int -> [Token] -> [Token]
    pushDigit !beg !end !acc
        | tfNumber =
            let !tok = mkTokenDigit (TI.Text arr beg (end - beg))
             in tok : acc
        | otherwise = acc

    {-# INLINE pushBracket #-}
    pushBracket :: Int -> Int -> [Token] -> [Token]
    pushBracket !beg !end !acc
        | tfBracket =
            let !tok = mkTokenBracket (TI.Text arr beg (end - beg))
             in tok : acc
        | otherwise = acc

    {-# INLINE pushOperator #-}
    pushOperator :: Int -> Int -> [Token] -> [Token]
    pushOperator !beg !end !acc
        | tfOperator =
            let !tok = mkTokenOperator (TI.Text arr beg (end - beg))
             in tok : acc
        | otherwise = acc

    {-# INLINE pushString #-}
    pushString :: Int -> Int -> [Token] -> [Token]
    pushString !beg !end !acc
        | tfString =
            let !tok = mkTokenString (TI.Text arr beg (end - beg))
             in tok : acc
        | otherwise = acc

    goSpace :: Int -> [Token] -> [Token]
    goSpace !ix !acc
        | ix >= endOff = reverse acc
        | otherwise =
            let !b0 = A.unsafeIndex arr ix
             in if b0 == 0x20
                    then goSpace (ix + 1) acc
                    else
                        let !(# !x, !delta #) =
                                if b0 < 0x80
                                    then (# unsafeChr8 b0, 1 #)
                                    else
                                        let TU.Iter !c !d = TU.iter txt (ix - srcOff)
                                         in (# c, d #)
                            !nextIx = ix + delta
                         in if
                                | isSpace x -> goSpace nextIx acc
                                | inline (isValidChar @cs1 x) -> goIdent ix nextIx acc
                                | x == startLiteralMarker -> goLiteral nextIx acc
                                | isDigit x -> goDigit ix nextIx acc
                                | isBracket' x -> goSpace nextIx (pushBracket ix nextIx acc)
                                | otherwise -> goOther ix nextIx acc

    goIdent :: Int -> Int -> [Token] -> [Token]
    goIdent !tokStart !ix !acc
        | ix >= endOff = reverse (pushWord tokStart endOff acc)
        | otherwise =
            let !(# !x, !delta #) = decodeAt ix
                !nextIx = ix + delta
             in if inline (isValidChar @cs x)
                    then goIdent tokStart nextIx acc
                    else
                        let !acc1 = pushWord tokStart ix acc
                         in if
                                | isSpace x -> goSpace nextIx acc1
                                | x == startLiteralMarker -> goLiteral nextIx acc1
                                | isBracket' x -> goSpace nextIx (pushBracket ix nextIx acc1)
                                | otherwise -> goOther ix nextIx acc1

    goDigit :: Int -> Int -> [Token] -> [Token]
    goDigit !tokStart !ix !acc
        | ix >= endOff = reverse (pushDigit tokStart endOff acc)
        | otherwise =
            let !(# !x, !delta #) = decodeAt ix
                !nextIx = ix + delta
             in if isCharNumber x
                    then goDigit tokStart nextIx acc
                    else
                        let !acc1 = pushDigit tokStart ix acc
                         in if
                                | isSpace x -> goSpace nextIx acc1
                                | x == startLiteralMarker -> goLiteral nextIx acc1
                                | inline (isValidChar @cs1 x) -> goIdent ix nextIx acc1
                                | isBracket' x -> goSpace nextIx (pushBracket ix nextIx acc1)
                                | otherwise -> goOther ix nextIx acc1

    goLiteral :: Int -> [Token] -> [Token]
    goLiteral !litStart !acc
        | litStart >= endOff = reverse acc
        | otherwise =
            case memchr arr# litStart (endOff - litStart) endLiteralMarkerByte of
                -1 -> reverse (pushString litStart endOff acc)
                relOff ->
                    let !endMarkIx = litStart + relOff
                     in goSpace (endMarkIx + 1) (pushString litStart endMarkIx acc)

    goOther :: Int -> Int -> [Token] -> [Token]
    goOther !tokStart !ix !acc
        | ix >= endOff = reverse (pushOperator tokStart endOff acc)
        | otherwise =
            let !(# !x, !delta #) = decodeAt ix
                !nextIx = ix + delta
             in if
                    | isSpace x -> goSpace nextIx (pushOperator tokStart ix acc)
                    | inline (isValidChar @cs1 x) -> goIdent ix nextIx (pushOperator tokStart ix acc)
                    | isDigit x ->
                        if ix - tokStart == 1 && A.unsafeIndex arr tokStart == 0x2E
                            then goDigit tokStart nextIx acc
                            else goDigit ix nextIx (pushOperator tokStart ix acc)
                    | isBracket' x ->
                        let !acc1 = pushOperator tokStart ix acc
                         in goSpace nextIx (pushBracket ix nextIx acc1)
                    | x == startLiteralMarker -> goLiteral nextIx (pushOperator tokStart ix acc)
                    | isPunctuation x -> goOther ix nextIx (pushOperator tokStart ix acc)
                    | otherwise ->
                        if strict
                            then goOther tokStart nextIx acc
                            else goOther ix nextIx (pushOperator tokStart ix acc)

-- ---------------------------------------------------------------------------
-- Reference STRef + Seq implementation (retained for QuickCheck & corpus
-- differential testing).
-- ---------------------------------------------------------------------------

newtype TokenState = TokenState {_unTokenState :: Int}
    deriving newtype (Eq)

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

(<~) :: STRef s a -> a -> ST s ()
ref <~ !x = writeSTRef ref x
{-# INLINE (<~) #-}

data TokenIdx = TokenIdx
    { _offset :: {-# UNPACK #-} !Int
    , len :: {-# UNPACK #-} !Int
    }

tkString :: TokenIdx -> T.Text -> T.Text
tkString (TokenIdx off l) = TU.takeWord8 l . TU.dropWord8 off
{-# INLINE tkString #-}

data AccOp = Reset | Start {-# UNPACK #-} !Int !Int | Append {-# UNPACK #-} !Int !Int

(<<~) :: STRef s TokenIdx -> AccOp -> ST s ()
ref <<~ Reset = writeSTRef ref (TokenIdx (-1) 0)
ref <<~ Start offset delta = writeSTRef ref (TokenIdx offset delta)
ref <<~ Append offset delta = modifySTRef' ref $ \case
    TokenIdx (-1) 0 -> TokenIdx offset delta
    TokenIdx offset' delta' -> TokenIdx offset' (delta + delta')
{-# INLINE (<<~) #-}

parseTokensRef :: TokenFilter -> Maybe FileTypeInfo -> Bool -> T.Text -> [Token]
parseTokensRef tf info strict txt =
    filter (not . isTokenUnspecified) . toList $ runST $ do
        let (runtimeCS1, runtimeCS2) = fromMaybe (None, None) (info >>= ftIdentCharSet)
        case (runtimeCS1, runtimeCS2) of
            (None, None) -> parseTokenRef' @'None @'None tf info True txt
            (Alpha_, AlphaNum_') -> parseTokenRef' @'Alpha_ @'AlphaNum_' tf info strict txt
            (Alpha_, AlphaNum_) -> parseTokenRef' @'Alpha_ @'AlphaNum_ tf info strict txt
            (Alpha, AlphaNum_) -> parseTokenRef' @'Alpha @'AlphaNum_ tf info strict txt
            (Alpha_, AlphaNumDash_) -> parseTokenRef' @'Alpha_ @'AlphaNumDash_ tf info strict txt
            (AlphaDash_, AlphaNumDash_) -> parseTokenRef' @'AlphaDash_ @'AlphaNumDash_ tf info strict txt
            (ClojureIdentStart, ClojureIdentCont) -> parseTokenRef' @'ClojureIdentStart @'ClojureIdentCont tf info strict txt
            (CSharpIdentStart, CSharpIdentCont) -> parseTokenRef' @'CSharpIdentStart @'CSharpIdentCont tf info strict txt
            (HtmlIdentStart, HtmlIdentCont) -> parseTokenRef' @'HtmlIdentStart @'HtmlIdentCont tf info strict txt
            (JavaIdentStart, JavaIdentCont) -> parseTokenRef' @'JavaIdentStart @'JavaIdentCont tf info strict txt
            (JuliaIdentStart, JuliaIdentCont) -> parseTokenRef' @'JuliaIdentStart @'JuliaIdentCont tf info strict txt
            (ListIdent, ListIdent) -> parseTokenRef' @'ListIdent @'ListIdent tf info strict txt
            (Unicode_, UnicodeNum_) -> parseTokenRef' @'Unicode_ @'UnicodeNum_ tf info strict txt
            (UnicodeDollar_, UnicodeNumDollar_) -> parseTokenRef' @'UnicodeDollar_ @'UnicodeNumDollar_ tf info strict txt
            (Unicode_, UnicodeNum_') -> parseTokenRef' @'Unicode_ @'UnicodeNum_' tf info strict txt
            (Unicode_, UnicodeNumDollar_) -> parseTokenRef' @'Unicode_ @'UnicodeNumDollar_ tf info strict txt
            (UnicodeXIDStart_, UnicodeNumXIDCont_) -> parseTokenRef' @'UnicodeXIDStart_ @'UnicodeNumXIDCont_ tf info strict txt
            (AgdaIdent, AgdaIdent) -> parseTokenRef' @'AgdaIdent @'AgdaIdent tf info strict txt
            _ -> error $ "CGrep: unsupported CharSet combination: " <> show (runtimeCS1, runtimeCS2)

parseTokenRef' :: forall (cs1 :: CharSet) (cs :: CharSet) a. (IsCharSet cs1, IsCharSet cs) => TokenFilter -> Maybe FileTypeInfo -> Bool -> T.Text -> ST a (S.Seq Token)
parseTokenRef' tf@TokenFilter{..} info strict txt = do
    stateR <- newSTRef StateSpace
    accR <- newSTRef (TokenIdx (-1) 0)
    tokensR <- newSTRef S.empty

    iterM txt $ \(# x, cur, delta #) -> do
        state <- readSTRef stateR
        case state of
            StateSpace ->
                if
                    | isSpace x -> do accR <<~ Reset
                    | inline (isValidChar @cs1 x) -> do stateR <~ StateIdentifier; accR <<~ Start cur delta
                    | x == startLiteralMarker -> do stateR <~ StateLiteral; accR <<~ Start (cur + delta) 0
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
                            | x == startLiteralMarker -> do stateR <~ StateLiteral; accR <<~ Start (cur + delta) 0; tokensR <~ (tokens |> buildTokenIf tfIdentifier tfKeyword tfNativeType (mkTokenFromWord info) acc txt)
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
                            | x == startLiteralMarker -> do stateR <~ StateLiteral; accR <<~ Start (cur + delta) 0; tokensR <~ (tokens |> buildToken tfNumber mkTokenDigit acc txt)
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
                        | x == startLiteralMarker -> do stateR <~ StateLiteral; accR <<~ Start (cur + delta) 0; tokensR <~ (tokens |> buildToken tfBracket mkTokenBracket acc txt)
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
                        | x == startLiteralMarker -> do stateR <~ StateLiteral; accR <<~ Start (cur + delta) 0; tokensR <~ (tokens |> buildToken tfOperator mkTokenOperator acc txt)
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
buildFilteredToken tf f (TokenIdx start l) txt =
    let t = f (textSlice txt start l)
     in if filterToken tf t
            then t
            else unspecifiedToken
{-# INLINE buildFilteredToken #-}

buildToken :: Bool -> (T.Text -> Token) -> TokenIdx -> T.Text -> Token
buildToken True f (TokenIdx start l) txt = f (textSlice txt start l)
buildToken False _ (TokenIdx _start _len) _txt = unspecifiedToken
{-# INLINE buildToken #-}

buildTokenIf :: Bool -> Bool -> Bool -> (T.Text -> Token) -> TokenIdx -> T.Text -> Token
buildTokenIf i k t f (TokenIdx start l) txt =
    if i && isTokenIdentifier tok || k && isTokenKeyword tok || t && isTokenNativeType tok
        then tok
        else unspecifiedToken
  where
    tok = f (textSlice txt start l)

unspecifiedToken :: Token
unspecifiedToken = Token $ Chunk ChunkUnspec T.empty
{-# INLINE unspecifiedToken #-}

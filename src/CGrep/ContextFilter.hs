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
{-# LANGUAGE OverloadedStrings #-}

module CGrep.ContextFilter where

import CGrep.Boundary (Boundary (..))
import CGrep.Parser.Char (chr, isSpace)
import CGrep.Text (blankByWidth)
import Data.Bits (Bits (complement, shiftL, shiftR, xor, (.&.), (.|.)))
import Data.Int (Int32)
import Data.List (find, findIndex, nub, sort)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Internal.Fusion as TIF
import qualified Data.Text.Internal.Fusion.Common as TIFC
import Options (Options (..))
import Util (findWithIndex)

type FilterFunction = ContextFilter -> TIF.Stream Char -> TIF.Stream Char


data Context = Code | Comment | Literal
    deriving stock (Eq, Show)

newtype ContextBit = ContextBit Int32
    deriving stock (Show)
    deriving newtype (Eq, Bits)

contextBitEmpty :: ContextBit
contextBitEmpty = ContextBit 0

contextBitCode :: ContextBit
contextBitCode = ContextBit 0x1

contextBitComment :: ContextBit
contextBitComment = ContextBit 0x2

contextBitLiteral :: ContextBit
contextBitLiteral = ContextBit 0x4

(~=) :: ContextBit -> Bool -> ContextBit
b ~= True = b
_ ~= False = contextBitEmpty
{-# INLINE (~=) #-}

(~?) :: ContextFilter -> ContextBit -> Bool
f ~? b = (unFilter f .&. b) /= contextBitEmpty
{-# INLINE (~?) #-}

(~!) :: ContextFilter -> ContextBit -> ContextFilter
a ~! b = ContextFilter $ unFilter a .&. complement b
{-# INLINE (~!) #-}

newtype ContextFilter = ContextFilter {unFilter :: ContextBit}
    deriving stock (Show)
    deriving newtype (Eq, Bits)

contextFilterAll :: ContextFilter
contextFilterAll = ContextFilter (contextBitCode .|. contextBitComment .|. contextBitLiteral)
{-# NOINLINE contextFilterAll #-}

isContextFilterAll :: ContextFilter -> Bool
isContextFilterAll f = f == contextFilterAll
{-# INLINE isContextFilterAll #-}

mustRunContextFilter :: ContextFilter -> Bool
mustRunContextFilter = not . isContextFilterAll
{-# INLINE mustRunContextFilter #-}

codeFilter :: ContextFilter -> Bool
codeFilter f = (unFilter f .&. contextBitCode) /= contextBitEmpty
{-# INLINE codeFilter #-}

commentFilter :: ContextFilter -> Bool
commentFilter f = (unFilter f .&. contextBitComment) /= contextBitEmpty
{-# INLINE commentFilter #-}

literalFilter :: ContextFilter -> Bool
literalFilter f = (unFilter f .&. contextBitLiteral) /= contextBitEmpty
{-# INLINE literalFilter #-}

data StreamB

data StreamBoundary = StreamBoundary
    { sbBegin :: (# Char, TIF.Stream Char #)
    , sbBeginLen :: Int
    , sbEnd :: (# Char, TIF.Stream Char #)
    , sbEndLen :: Int
    }

toStreamBoundary :: Boundary -> StreamBoundary
toStreamBoundary Boundary{..} =
    StreamBoundary
        { sbBegin = (# T.head bBegin, TIF.stream (T.tail bBegin) #)
        , sbBeginLen = bBeginLen
        , sbEnd = (# T.head bEnd, TIF.stream (T.tail bEnd) #)
        , sbEndLen = bEndLen
        }

data ParConfig = ParConfig
    { commBound :: [StreamBoundary]
    , litrBound :: [StreamBoundary]
    , rawBound :: [StreamBoundary]
    , chrBound :: [StreamBoundary]
    , inits :: !T.Text
    , alterBoundary :: !Bool
    }

safeHead :: T.Text -> T.Text
safeHead txt = case T.uncons txt of
    Just (x, _) -> T.singleton x
    Nothing -> T.empty
{-# INLINE safeHead #-}

mkParConfig :: [Boundary] -> [Boundary] -> [Boundary] -> [Boundary] -> Bool -> ParConfig
mkParConfig cs ls rs chs ab =
    ParConfig
        { commBound = toStreamBoundary <$> cs
        , litrBound = toStreamBoundary <$> ls
        , rawBound = toStreamBoundary <$> rs
        , chrBound = toStreamBoundary <$> chs
        , inits =
            mconcat . nub . sort $
                ((safeHead . bBegin) <$> cs)
                    <> ((safeHead . bBegin) <$> ls)
                    <> ((safeHead . bBegin) <$> rs)
                    <> ((safeHead . bBegin) <$> chs)
        , alterBoundary = ab
        }

data ParState = ParState
    { ctxState :: !ContextState
    , nextState :: !ContextState
    , display :: !Bool
    , skip :: {-# UNPACK #-} !Int
    }
    deriving stock (Show)

data ContextState
    = CodeState1
    | CodeStateN
    | CommState1 {-# UNPACK #-} !Int
    | CommStateN {-# UNPACK #-} !Int
    | ChrState {-# UNPACK #-} !Int
    | LitrState1 {-# UNPACK #-} !Int
    | LitrStateN {-# UNPACK #-} !Int
    | RawState {-# UNPACK #-} !Int
    deriving stock (Show, Eq, Ord)

mkContextFilter :: Options -> ContextFilter
mkContextFilter Options{..} =
    if not (code || comment || literal)
        then contextFilterAll
        else ContextFilter $ contextBitCode ~= code .|. contextBitComment ~= comment .|. contextBitLiteral ~= literal

getContext :: ContextState -> Context
getContext CodeState1 = Code
getContext CodeStateN = Code
getContext (CommState1 _) = Comment
getContext (CommStateN _) = Comment
getContext (LitrState1 _) = Literal
getContext (LitrStateN _) = Literal
getContext (RawState _) = Literal
getContext (ChrState _) = Literal
{-# INLINE getContext #-}

-- contextFilterFun:
--

start_literal :: Char
start_literal = chr 2

end_literal :: Char
end_literal = chr 3

data ParData = ParData
    { pdText :: TIF.Stream Char
    , pdState :: !ParState
    }

runContextFilter :: ParConfig -> ContextFilter -> TIF.Stream Char -> TIF.Stream Char
runContextFilter conf@ParConfig{..} f txt
    | alterBoundary = TIFC.unfoldr (contextFilter' conf) (ParData txt (ParState CodeState1 CodeState1 (codeFilter f) 0))
    | otherwise = TIFC.unfoldr (contextFilter'' conf) (ParData txt (ParState CodeState1 CodeState1 (codeFilter f) 0))
  where
    contextFilter' :: ParConfig -> ParData -> Maybe (Char, ParData)
    contextFilter' conf (ParData (TIFC.uncons -> Just (x, xs)) s) =
        let s' = nextContextState conf s x xs f
         in if display s'
                then case (# getContext (ctxState s), getContext (ctxState s') #) of
                    (# Code, Literal #) -> Just (start_literal, ParData xs s')
                    (# Literal, Code #) -> Just (end_literal, ParData xs s')
                    _ -> Just (x, ParData xs s')
                else
                    if isSpace x
                        then Just (x, ParData xs s')
                        else Just (blankByWidth x, ParData xs s')
    contextFilter' _ (ParData (TIFC.uncons -> Nothing) _) = Nothing

    contextFilter'' :: ParConfig -> ParData -> Maybe (Char, ParData)
    contextFilter'' conf (ParData (TIFC.uncons -> Just (x, xs)) s) =
        let s' = nextContextState conf s x xs f
         in if display s' || isSpace x
                then Just (x, ParData xs s')
                else Just (blankByWidth x, ParData xs s')
    contextFilter'' _ (ParData (TIFC.uncons -> Nothing) _) = Nothing

-- DUMMY
-- contextFilter'' :: ParConfig -> ParData -> Maybe (Char, ParData)
-- contextFilter'' c (ParData txt@(T.uncons -> Just (x, xs)) s) = Just (x, ParData xs s)
-- contextFilter'' _ (ParData (T.uncons -> Nothing) _) = Nothing

nextContextState :: ParConfig -> ParState -> Char -> TIF.Stream Char -> ContextFilter -> ParState
nextContextState conf s@ParState{..} c cont f
    | skip > 0 = {-# SCC "skip" #-} transState s{skip = skip - 1}
    | CodeState1 <- ctxState =
        {-# SCC "next_code1" #-}
        if c `T.elem` inits conf
            then case findPrefixBoundary c cont (commBound conf) of
                (# i, Just b #) -> {-# SCC "next_code1_1" #-} transState s{nextState = CommState1 i, display = commentFilter f, skip = (sbBeginLen b) - 1}
                _ -> case findPrefixBoundary c cont (litrBound conf) of
                    (# i, Just b #) -> {-# SCC "next_code1_2" #-} transState s{nextState = LitrState1 i, display = codeFilter f, skip = (sbBeginLen b) - 1}
                    _ -> case findPrefixBoundary c cont  (rawBound conf) of
                        (# i, Just b #) -> {-# SCC "next_code1_3" #-} transState s{nextState = RawState i, display = codeFilter f, skip = (sbBeginLen b) - 1}
                        _ -> case findPrefixBoundary' c cont (chrBound conf) of
                            (# i, Just b #) -> transState s{nextState = ChrState i, display = codeFilter f, skip = (sbBeginLen b) - 1}
                            _ -> {-# SCC "next_code1_5" #-} s{ctxState = CodeStateN, nextState = CodeStateN, display = codeFilter f, skip = 0}
            else {-# SCC "next_code1_0" #-} s{ctxState = CodeStateN, nextState = CodeStateN, display = codeFilter f, skip = 0}
    | CodeStateN <- ctxState =
        {-# SCC "next_code" #-}
        if {-# SCC "next_code_if" #-} c `T.elem` inits conf
            then
                {-# SCC "next_code_then" #-}
                case findPrefixBoundary c cont (commBound conf) of
                    (# i, Just b #) -> {-# SCC "next_code1_1" #-} transState s{nextState = CommState1 i, display = commentFilter f, skip = (sbBeginLen b) - 1}
                    _ -> case findPrefixBoundary c cont (litrBound conf) of
                        (# i, Just b #) -> {-# SCC "next_code1_2" #-} transState s{nextState = LitrState1 i, display = codeFilter f, skip = (sbBeginLen b) - 1}
                        _ -> case findPrefixBoundary c cont (rawBound conf) of
                            (# i, Just b #) -> {-# SCC "next_code1_3" #-} transState s{nextState = RawState i, display = codeFilter f, skip = (sbBeginLen b) - 1}
                            _ -> case findPrefixBoundary' c cont (chrBound conf) of
                                (# i, Just b #) -> transState s{nextState = ChrState i, display = codeFilter f, skip = (sbBeginLen b) - 1}
                                _ -> {-# SCC "next_code_5" #-} s
            else {-# SCC "next_code_else" #-} s
    | CommState1 n <- ctxState =
        let !StreamBoundary{..} = commBound conf !! n
            (# ec, es #) = sbEnd
         in {-# SCC "next_comm1" #-}
            if c == ec && es `TIFC.isPrefixOf` cont
                then transState $ s{nextState = CodeState1, display = commentFilter f, skip = sbEndLen - 1}
                else s{ctxState = CommStateN n, nextState = CommStateN n, display = commentFilter f, skip = 0}
    | CommStateN n <- ctxState =
        let !StreamBoundary{..} = commBound conf !! n
            (# ec, es #) = sbEnd
         in {-# SCC "next_comm" #-}
            if c == ec && es `TIFC.isPrefixOf` cont
                then transState $ s{nextState = CodeState1, display = commentFilter f, skip = sbEndLen - 1}
                else s
    | LitrState1 n <- ctxState =
        if c == '\\'
            then s{display = displayContext ctxState f, skip = 1}
            else
                let !StreamBoundary{..} = litrBound conf !! n
                    (# ec, es #) = sbEnd
                 in {-# SCC "next_liter" #-}
                    if c == ec && es `TIFC.isPrefixOf` cont
                        then s{ctxState = CodeState1, nextState = CodeState1, display = codeFilter f, skip = sbEndLen - 1}
                        else s{ctxState = LitrStateN n, nextState = LitrStateN n, display = literalFilter f, skip = 0}
    | LitrStateN n <- ctxState =
        if c == '\\'
            then s{display = displayContext ctxState f, skip = 1}
            else
                let !StreamBoundary{..} = litrBound conf !! n
                    (# ec, es #) = sbEnd
                 in {-# SCC "next_liter" #-}
                    if c == ec && es `TIFC.isPrefixOf` cont
                        then s{ctxState = CodeState1, nextState = CodeState1, display = codeFilter f, skip = sbEndLen - 1}
                        else s
    | ChrState n <- ctxState =
        if c == '\\'
            then s{display = displayContext ctxState f, skip = 1}
            else
                let !StreamBoundary{..} = chrBound conf !! n
                    (# ec, es #) = sbEnd
                 in {-# SCC "next_chr" #-}
                    if c == ec && es `TIFC.isPrefixOf` cont
                        then s{ctxState = CodeState1, nextState = CodeState1, display = codeFilter f, skip = sbEndLen - 1}
                        else s{display = literalFilter f, skip = 0}
    | RawState n <- ctxState =
        let !StreamBoundary{..} = rawBound conf !! n
            (# ec, es #) = sbEnd
         in {-# SCC "next_raw" #-}
            if c == ec && es `TIFC.isPrefixOf` cont
                then s{ctxState = CodeState1, nextState = CodeState1, display = codeFilter f, skip = sbEndLen - 1}
                else s{display = literalFilter f, skip = 0}

displayContext :: ContextState -> ContextFilter -> Bool
displayContext CodeState1 cf = cf ~? contextBitCode
displayContext CodeStateN cf = cf ~? contextBitCode
displayContext (CommState1 _) cf = cf ~? contextBitComment
displayContext (CommStateN _) cf = cf ~? contextBitComment
displayContext (LitrState1 _) cf = cf ~? contextBitLiteral
displayContext (LitrStateN _) cf = cf ~? contextBitLiteral
displayContext (RawState _) cf = cf ~? contextBitLiteral
displayContext (ChrState _) cf = cf ~? contextBitLiteral
{-# INLINE displayContext #-}

transState :: ParState -> ParState
transState s@ParState{..}
    | skip == 0 = s{ctxState = nextState}
    | otherwise = s
{-# INLINE transState #-}

findPrefixBoundary :: Char -> TIF.Stream Char -> [StreamBoundary] -> (# Int, Maybe StreamBoundary #)
findPrefixBoundary x xs vb =
    findWithIndex
        ( \(StreamBoundary{..}) ->
            let (# c, cont #) = sbBegin
             in c == x && cont `TIFC.isPrefixOf` xs
        )
        vb
{-# INLINE findPrefixBoundary #-}

findPrefixBoundary' :: Char -> TIF.Stream Char -> [StreamBoundary] -> (# Int, Maybe StreamBoundary #)
findPrefixBoundary' x xs vb =
    case findWithIndex (\(StreamBoundary{..}) -> let (# c, cont #) = sbBegin in c == x && cont `TIFC.isPrefixOf` xs) vb of
        elm@(# _, Just StreamBoundary{..} #) -> case xs of
            (TIFC.uncons -> Just (y, ys)) ->
                let skip = if y == '\\' then 1 else 0
                 in case TIFC.drop skip ys of
                        (TIFC.uncons -> Just (z, zs)) ->
                            let (#e, es #) = sbEnd
                             in if z == e && es `TIFC.isPrefixOf` zs
                                then elm
                                else (# 0, Nothing #)
                        _ -> (# 0, Nothing #)
            _ -> (# 0, Nothing #)
        _ -> (# 0, Nothing #)

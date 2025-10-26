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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module CGrep.ContextFilter (
    FilterFunction,
    ContextFilter (..),
    isContextFilterAll,
    contextBitCode,
    contextBitComment,
    contextBitLiteral,
    mkContextFilter,
    mkParConfig,
    (~!),
    runContextFilter,
    start_literal,
    end_literal,
) where

import CGrep.Boundary (Boundary (..))
import CGrep.Parser.Char (chr, isSpace)
import CGrep.Text (blankByWidth)
import Data.Bits (Bits (complement, (.&.), (.|.)))
import Data.Int (Int32)
import Data.List (nub, sort)
import qualified Data.Text as T
import qualified Data.Vector as V
import Options (Options (..))

type FilterFunction = ContextFilter -> T.Text -> T.Text

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

codeFilter :: ContextFilter -> Bool
codeFilter f = (unFilter f .&. contextBitCode) /= contextBitEmpty
{-# INLINE codeFilter #-}

commentFilter :: ContextFilter -> Bool
commentFilter f = (unFilter f .&. contextBitComment) /= contextBitEmpty
{-# INLINE commentFilter #-}

literalFilter :: ContextFilter -> Bool
literalFilter f = (unFilter f .&. contextBitLiteral) /= contextBitEmpty
{-# INLINE literalFilter #-}

data StreamBoundary = StreamBoundary
    { sbBegin :: !(# Char, T.Text #)
    , sbBeginLen :: {-# UNPACK #-} !Int
    , sbEnd :: !(# Char, T.Text #)
    , sbEndLen :: {-# UNPACK #-} !Int
    }

toStreamBoundary :: Boundary -> StreamBoundary
toStreamBoundary Boundary{..} =
    StreamBoundary
        { sbBegin = (# T.head bBegin, T.tail bBegin #)
        , sbBeginLen = bBeginLen
        , sbEnd = (# T.head bEnd, T.tail bEnd #)
        , sbEndLen = bEndLen
        }

data ParConfig = ParConfig
    { commBound :: !(V.Vector StreamBoundary)
    , litrBound :: !(V.Vector StreamBoundary)
    , rawBound :: !(V.Vector StreamBoundary)
    , chrBound :: !(V.Vector StreamBoundary)
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
        { commBound = V.fromList $ toStreamBoundary <$> cs
        , litrBound = V.fromList $ toStreamBoundary <$> ls
        , rawBound = V.fromList $ toStreamBoundary <$> rs
        , chrBound = V.fromList $ toStreamBoundary <$> chs
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

-- Strict ParData to minimize allocations
data ParData = ParData
    { _pdText :: !T.Text
    , _pdState :: !ParState
    }

runContextFilter :: ParConfig -> ContextFilter -> T.Text -> T.Text
runContextFilter conf@ParConfig{..} f txt
    | alterBoundary = T.unfoldr contextFilter' initialData
    | otherwise = T.unfoldr contextFilter'' initialData
  where
    !initialData = ParData txt $! ParState CodeState1 CodeState1 (codeFilter f) 0

    contextFilter' :: ParData -> Maybe (Char, ParData)
    contextFilter' !(ParData stream !state) = case T.uncons stream of
        Just (!x, !xs) ->
            let !nextState = nextContextState conf state x xs f
                !disp = display nextState
             in if disp
                    then case (# getContext (ctxState state), getContext (ctxState nextState) #) of
                        (# Code, Literal #) -> Just (start_literal, ParData xs nextState)
                        (# Literal, Code #) -> Just (end_literal, ParData xs nextState)
                        _ -> Just (x, ParData xs nextState)
                    else
                        let !isSp = isSpace x
                         in if isSp
                                then Just (x, ParData xs nextState)
                                else Just (blankByWidth x, ParData xs nextState)
        Nothing -> Nothing
    {-# INLINE contextFilter' #-}

    contextFilter'' :: ParData -> Maybe (Char, ParData)
    contextFilter'' !(ParData stream !state) = case T.uncons stream of
        Just (!x, !xs) ->
            let !nextState = nextContextState conf state x xs f
                !disp = display nextState
                !isSp = isSpace x
             in if disp || isSp
                    then Just (x, ParData xs nextState)
                    else Just (blankByWidth x, ParData xs nextState)
        Nothing -> Nothing
    {-# INLINE contextFilter'' #-}

nextContextState :: ParConfig -> ParState -> Char -> T.Text -> ContextFilter -> ParState
nextContextState !conf s@ParState{..} !c !cont !f
    | skip > 0 = transState s{skip = skip - 1}
    | CodeState1 <- ctxState =
        if c `T.elem` inits conf
            then case findPrefixBoundary c cont (commBound conf) of
                (# i, Just !b #) -> transState s{nextState = CommState1 i, display = commentFilter f, skip = (sbBeginLen b) - 1}
                _ -> case findPrefixBoundary c cont (litrBound conf) of
                    (# i, Just !b #) -> transState s{nextState = LitrState1 i, display = codeFilter f, skip = (sbBeginLen b) - 1}
                    _ -> case findPrefixBoundary c cont (rawBound conf) of
                        (# i, Just !b #) -> transState s{nextState = RawState i, display = codeFilter f, skip = (sbBeginLen b) - 1}
                        _ -> case findPrefixBoundary' c cont (chrBound conf) of
                            (# i, Just !b #) -> transState s{nextState = ChrState i, display = codeFilter f, skip = (sbBeginLen b) - 1}
                            _ -> s{ctxState = CodeStateN, nextState = CodeStateN, display = codeFilter f, skip = 0}
            else s{ctxState = CodeStateN, nextState = CodeStateN, display = codeFilter f, skip = 0}
    | CodeStateN <- ctxState =
        if c `T.elem` inits conf
            then case findPrefixBoundary c cont (commBound conf) of
                (# i, Just !b #) -> transState s{nextState = CommState1 i, display = commentFilter f, skip = (sbBeginLen b) - 1}
                _ -> case findPrefixBoundary c cont (litrBound conf) of
                    (# i, Just !b #) -> transState s{nextState = LitrState1 i, display = codeFilter f, skip = (sbBeginLen b) - 1}
                    _ -> case findPrefixBoundary c cont (rawBound conf) of
                        (# i, Just !b #) -> transState s{nextState = RawState i, display = codeFilter f, skip = (sbBeginLen b) - 1}
                        _ -> case findPrefixBoundary' c cont (chrBound conf) of
                            (# i, Just !b #) -> transState s{nextState = ChrState i, display = codeFilter f, skip = (sbBeginLen b) - 1}
                            _ -> s
            else s
    | CommState1 n <- ctxState =
        let !StreamBoundary{..} = V.unsafeIndex (commBound conf) n
            (# ec, es #) = sbEnd
         in if c == ec && es `T.isPrefixOf` cont
                then transState s{nextState = CodeState1, display = commentFilter f, skip = sbEndLen - 1}
                else s{ctxState = CommStateN n, nextState = CommStateN n, display = commentFilter f, skip = 0}
    | CommStateN n <- ctxState =
        let !StreamBoundary{..} = V.unsafeIndex (commBound conf) n
            (# ec, es #) = sbEnd
         in if c == ec && es `T.isPrefixOf` cont
                then transState s{nextState = CodeState1, display = commentFilter f, skip = sbEndLen - 1}
                else s
    | LitrState1 n <- ctxState =
        if c == '\\'
            then s{display = displayContext ctxState f, skip = 1}
            else
                let !StreamBoundary{..} = V.unsafeIndex (litrBound conf) n
                    (# ec, es #) = sbEnd
                 in if c == ec && es `T.isPrefixOf` cont
                        then s{ctxState = CodeState1, nextState = CodeState1, display = codeFilter f, skip = sbEndLen - 1}
                        else s{ctxState = LitrStateN n, nextState = LitrStateN n, display = literalFilter f, skip = 0}
    | LitrStateN n <- ctxState =
        if c == '\\'
            then s{display = displayContext ctxState f, skip = 1}
            else
                let !StreamBoundary{..} = V.unsafeIndex (litrBound conf) n
                    (# ec, es #) = sbEnd
                 in if c == ec && es `T.isPrefixOf` cont
                        then s{ctxState = CodeState1, nextState = CodeState1, display = codeFilter f, skip = sbEndLen - 1}
                        else s
    | ChrState n <- ctxState =
        if c == '\\'
            then s{display = displayContext ctxState f, skip = 1}
            else
                let !StreamBoundary{..} = V.unsafeIndex (chrBound conf) n
                    (# ec, es #) = sbEnd
                 in if c == ec && es `T.isPrefixOf` cont
                        then s{ctxState = CodeState1, nextState = CodeState1, display = codeFilter f, skip = sbEndLen - 1}
                        else s{display = literalFilter f, skip = 0}
    | RawState n <- ctxState =
        let !StreamBoundary{..} = V.unsafeIndex (rawBound conf) n
            (# ec, es #) = sbEnd
         in if c == ec && es `T.isPrefixOf` cont
                then s{ctxState = CodeState1, nextState = CodeState1, display = codeFilter f, skip = sbEndLen - 1}
                else s{display = literalFilter f, skip = 0}
{-# INLINE nextContextState #-}

displayContext :: ContextState -> ContextFilter -> Bool
displayContext !s !cf = case s of
    CodeState1 -> cf ~? contextBitCode
    CodeStateN -> cf ~? contextBitCode
    CommState1 _ -> cf ~? contextBitComment
    CommStateN _ -> cf ~? contextBitComment
    LitrState1 _ -> cf ~? contextBitLiteral
    LitrStateN _ -> cf ~? contextBitLiteral
    RawState _ -> cf ~? contextBitLiteral
    ChrState _ -> cf ~? contextBitLiteral
{-# INLINE displayContext #-}

transState :: ParState -> ParState
transState !s@ParState{..}
    | skip == 0 = s{ctxState = nextState}
    | otherwise = s
{-# INLINE transState #-}

findPrefixBoundary :: Char -> T.Text -> V.Vector StreamBoundary -> (# Int, Maybe StreamBoundary #)
findPrefixBoundary x xs vb = go 0
  where
    !len = V.length vb
    go !i
        | i >= len = (# 0, Nothing #)
        | otherwise =
            let !sb@StreamBoundary{..} = V.unsafeIndex vb i
                (# c, cont #) = sbBegin
             in if c == x && cont `T.isPrefixOf` xs
                    then (# i, Just sb #)
                    else go (i + 1)
{-# INLINE findPrefixBoundary #-}

findPrefixBoundary' :: Char -> T.Text -> V.Vector StreamBoundary -> (# Int, Maybe StreamBoundary #)
findPrefixBoundary' x xs vb = go 0
  where
    !len = V.length vb
    go !i
        | i >= len = (# 0, Nothing #)
        | otherwise =
            let !sb@StreamBoundary{..} = V.unsafeIndex vb i
                (# c, cont #) = sbBegin
             in if c == x && cont `T.isPrefixOf` xs
                    then case xs of
                        (T.uncons -> Just (y, ys)) ->
                            let skip = if y == '\\' then 1 else 0
                             in case T.drop skip ys of
                                    (T.uncons -> Just (z, zs)) ->
                                        let (# e, es #) = sbEnd
                                         in if z == e && es `T.isPrefixOf` zs
                                                then (# i, Just sb #)
                                                else go (i + 1)
                                    _ -> go (i + 1)
                        _ -> go (i + 1)
                    else go (i + 1)

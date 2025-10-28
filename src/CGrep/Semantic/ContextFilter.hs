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

module CGrep.Semantic.ContextFilter (
    FilterFunction,
    ContextFilter (..),
    isContextFilterAll,
    contextBitCode,
    contextBitComment,
    contextBitLiteral,
    mkContextFilter,
    mkParConfig,
    (~!),
    (~?),
    runContextFilter,
    startLiteralMarker,
    endLiteralMarker,
) where

import CGrep.Boundary (Boundary (..))
import CGrep.Parser.Char (chr, isSpace)
import CGrep.Text (blankByWidth)
import Data.Bits (Bits (complement, (.&.), (.|.)))
import Data.Int (Int32)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import Options (Options (..))

startLiteralMarker :: Char
startLiteralMarker = chr 2

endLiteralMarker :: Char
endLiteralMarker = chr 3

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

data RegionBoundary = RegionBoundary
    { rbBegin :: !(# Char, T.Text #)
    , rbBeginLen :: {-# UNPACK #-} !Int
    , rbEnd :: !(# Char, T.Text #)
    , rbEndLen :: {-# UNPACK #-} !Int
    }

toRegionBoundary :: Boundary -> RegionBoundary
toRegionBoundary Boundary{..} =
    RegionBoundary
        { rbBegin = (# T.head bBegin, T.tail bBegin #)
        , rbBeginLen = bBeginLen
        , rbEnd = (# T.head bEnd, T.tail bEnd #)
        , rbEndLen = bEndLen
        }

data ParConfig = ParConfig
    { commBound :: !(V.Vector RegionBoundary)
    , litrBound :: !(V.Vector RegionBoundary)
    , rawBound :: !(V.Vector RegionBoundary)
    , chrBound :: !(V.Vector RegionBoundary)
    , inits :: !T.Text
    , useMakers :: !Bool
    }

mkParConfig :: [Boundary] -> [Boundary] -> [Boundary] -> [Boundary] -> Bool -> ParConfig
mkParConfig cs ls rs chs ab =
    ParConfig
        { commBound = V.fromList $ toRegionBoundary <$> cs
        , litrBound = V.fromList $ toRegionBoundary <$> ls
        , rawBound = V.fromList $ toRegionBoundary <$> rs
        , chrBound = V.fromList $ toRegionBoundary <$> chs
        , inits =
            T.concat . Set.toList . Set.fromList $
                (safeHead . bBegin <$> cs)
                    <> (safeHead . bBegin <$> ls)
                    <> (safeHead . bBegin <$> rs)
                    <> (safeHead . bBegin <$> chs)
        , useMakers = ab
        }

data ParState = ParState
    { parCtxState :: !ContextState
    , parNextState :: !ContextState
    , parDisplay :: !Bool
    , parSkip :: {-# UNPACK #-} !Int
    , parText :: !T.Text
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

runContextFilter :: ParConfig -> ContextFilter -> T.Text -> T.Text
runContextFilter conf@ParConfig{..} f txt
    | useMakers = T.unfoldr contextFilter' initialState
    | otherwise = T.unfoldr contextFilter'' initialState
  where
    !initialState =
        ParState
            { parCtxState = CodeState1
            , parNextState = CodeState1
            , parDisplay = codeFilter f
            , parSkip = 0
            , parText = txt
            }

    contextFilter' :: ParState -> Maybe (Char, ParState)
    contextFilter' !state = case T.uncons (parText state) of
        Nothing -> Nothing
        Just (!x, !xs) ->
            let !nextState = nextContextState conf state x xs f
                !nextState' = nextState{parText = xs}
                !disp = parDisplay nextState
             in if disp
                    then
                        let !currCtx = getContext (parCtxState state)
                            !nextCtx = getContext (parCtxState nextState)
                         in case currCtx of
                                Code | nextCtx == Literal -> Just (startLiteralMarker, nextState')
                                Literal | nextCtx == Code -> Just (endLiteralMarker, nextState')
                                _ -> Just (x, nextState')
                    else Just (if isSpace x then x else blankByWidth x, nextState')
    {-# INLINE contextFilter' #-}

    contextFilter'' :: ParState -> Maybe (Char, ParState)
    contextFilter'' !state = case T.uncons (parText state) of
        Nothing -> Nothing
        Just (!x, !xs) ->
            let !nextState = nextContextState conf state x xs f
                !nextState' = nextState{parText = xs}
                !shouldDisplay = parDisplay nextState || isSpace x
             in Just (if shouldDisplay then x else blankByWidth x, nextState')
    {-# INLINE contextFilter'' #-}

nextContextState :: ParConfig -> ParState -> Char -> T.Text -> ContextFilter -> ParState
nextContextState !conf !s@ParState{..} !c !cont !f
    | parSkip > 0 =
        let !newSkip = parSkip - 1
         in if newSkip == 0
                then s{parCtxState = parNextState, parSkip = 0}
                else s{parSkip = newSkip}
    | CodeState1 <- parCtxState = handleCodeState True
    | CodeStateN <- parCtxState = handleCodeState False
    | CommState1 n <- parCtxState = handleCommentState n True
    | CommStateN n <- parCtxState = handleCommentState n False
    | LitrState1 n <- parCtxState = handleLiteralState n True
    | LitrStateN n <- parCtxState = handleLiteralState n False
    | ChrState n <- parCtxState = handleCharState n
    | RawState n <- parCtxState = handleRawState n
  where
    !codeDisp = codeFilter f
    !commDisp = commentFilter f
    !litrDisp = literalFilter f

    {-# INLINE transitionWith #-}
    transitionWith !nextSt !disp !skipLen =
        let !skip = skipLen - 1
         in if skip == 0
                then s{parCtxState = nextSt, parNextState = nextSt, parDisplay = disp, parSkip = 0}
                else s{parNextState = nextSt, parDisplay = disp, parSkip = skip}

    {-# INLINE handleCodeState #-}
    handleCodeState !isFirst =
        if c `T.elem` inits conf
            then case findPrefixBoundary c cont (commBound conf) of
                (# i, Just !b #) -> transitionWith (CommState1 i) commDisp (rbBeginLen b)
                _ -> case findPrefixBoundary c cont (litrBound conf) of
                    (# i, Just !b #) -> transitionWith (LitrState1 i) codeDisp (rbBeginLen b)
                    _ -> case findPrefixBoundary c cont (rawBound conf) of
                        (# i, Just !b #) -> transitionWith (RawState i) codeDisp (rbBeginLen b)
                        _ -> case findPrefixBoundary' c cont (chrBound conf) of
                            (# i, Just !b #) -> transitionWith (ChrState i) codeDisp (rbBeginLen b)
                            _ ->
                                if isFirst
                                    then s{parCtxState = CodeStateN, parNextState = CodeStateN, parDisplay = codeDisp, parSkip = 0}
                                    else s
            else
                if isFirst
                    then s{parCtxState = CodeStateN, parNextState = CodeStateN, parDisplay = codeDisp, parSkip = 0}
                    else s

    {-# INLINE handleCommentState #-}
    handleCommentState !n !isFirst =
        let !RegionBoundary{..} = V.unsafeIndex (commBound conf) n
            (# ec, es #) = rbEnd
         in if c == ec && es `T.isPrefixOf` cont
                then transitionWith CodeState1 commDisp rbEndLen
                else
                    if isFirst
                        then s{parCtxState = CommStateN n, parNextState = CommStateN n, parDisplay = commDisp, parSkip = 0}
                        else s

    {-# INLINE handleLiteralState #-}
    handleLiteralState !n !isFirst =
        if c == '\\'
            then s{parDisplay = litrDisp, parSkip = 1}
            else
                let !RegionBoundary{..} = V.unsafeIndex (litrBound conf) n
                    (# ec, es #) = rbEnd
                 in if c == ec && es `T.isPrefixOf` cont
                        then
                            let !skip = rbEndLen - 1
                             in if skip == 0
                                    then s{parCtxState = CodeState1, parNextState = CodeState1, parDisplay = codeDisp, parSkip = 0}
                                    else s{parCtxState = CodeState1, parNextState = CodeState1, parDisplay = codeDisp, parSkip = skip}
                        else
                            if isFirst
                                then s{parCtxState = LitrStateN n, parNextState = LitrStateN n, parDisplay = litrDisp, parSkip = 0}
                                else s

    {-# INLINE handleCharState #-}
    handleCharState !n =
        if c == '\\'
            then s{parDisplay = litrDisp, parSkip = 1}
            else
                let !RegionBoundary{..} = V.unsafeIndex (chrBound conf) n
                    (# ec, es #) = rbEnd
                 in if c == ec && es `T.isPrefixOf` cont
                        then
                            let !skip = rbEndLen - 1
                             in if skip == 0
                                    then s{parCtxState = CodeState1, parNextState = CodeState1, parDisplay = codeDisp, parSkip = 0}
                                    else s{parCtxState = CodeState1, parNextState = CodeState1, parDisplay = codeDisp, parSkip = skip}
                        else s{parDisplay = litrDisp, parSkip = 0}

    {-# INLINE handleRawState #-}
    handleRawState !n =
        let !RegionBoundary{..} = V.unsafeIndex (rawBound conf) n
            (# ec, es #) = rbEnd
         in if c == ec && es `T.isPrefixOf` cont
                then
                    let !skip = rbEndLen - 1
                     in if skip == 0
                            then s{parCtxState = CodeState1, parNextState = CodeState1, parDisplay = codeDisp, parSkip = 0}
                            else s{parCtxState = CodeState1, parNextState = CodeState1, parDisplay = codeDisp, parSkip = skip}
                else s{parDisplay = litrDisp, parSkip = 0}
{-# INLINE nextContextState #-}

findPrefixBoundary :: Char -> T.Text -> V.Vector RegionBoundary -> (# Int, Maybe RegionBoundary #)
findPrefixBoundary !x !xs !vb = go 0
  where
    !len = V.length vb
    go !i
        | i >= len = (# 0, Nothing #)
        | otherwise =
            let !RegionBoundary{..} = V.unsafeIndex vb i
                (# !c, !cont #) = rbBegin
             in if c == x
                    then
                        if cont `T.isPrefixOf` xs
                            then (# i, Just (V.unsafeIndex vb i) #)
                            else go (i + 1)
                    else go (i + 1)
{-# INLINE findPrefixBoundary #-}

findPrefixBoundary' :: Char -> T.Text -> V.Vector RegionBoundary -> (# Int, Maybe RegionBoundary #)
findPrefixBoundary' !x !xs !vb = go 0
  where
    !len = V.length vb
    go !i
        | i >= len = (# 0, Nothing #)
        | otherwise =
            let !RegionBoundary{..} = V.unsafeIndex vb i
                (# !c, !cont #) = rbBegin
             in if c /= x
                    then go (i + 1)
                    else
                        if not (cont `T.isPrefixOf` xs)
                            then go (i + 1)
                            else case xs of
                                (T.uncons -> Just (!y, !ys)) ->
                                    let !skip = if y == '\\' then 1 else 0
                                     in case T.drop skip ys of
                                            (T.uncons -> Just (!z, !zs)) ->
                                                let (# !e, !es #) = rbEnd
                                                 in if z == e && es `T.isPrefixOf` zs
                                                        then (# i, Just (V.unsafeIndex vb i) #)
                                                        else go (i + 1)
                                            _ -> go (i + 1)
                                _ -> go (i + 1)

safeHead :: T.Text -> T.Text
safeHead txt = case T.uncons txt of
    Just (x, _) -> T.singleton x
    Nothing -> T.empty
{-# INLINE safeHead #-}

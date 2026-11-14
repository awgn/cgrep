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
    contextFilterAll,
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

import CGrep.Boundary (Boundary (..), BoundaryGuard (..))
import CGrep.Parser.Char (chr, isAlphaNum, isSpace)
import CGrep.Text (blankByWidth)
import Data.Bits (Bits (complement, (.&.), (.|.)))
import Data.Int (Int32)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import Options (Options (..))

startLiteralMarker :: Char
startLiteralMarker = chr 2

endLiteralMarker :: Char
endLiteralMarker = chr 3

type FilterFunction = ContextFilter -> T.Text -> T.Text

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
    , rbEndUniform :: !Bool
    , rbGuard :: !BoundaryGuard
    }

toRegionBoundary :: Boundary -> Maybe RegionBoundary
toRegionBoundary Boundary{..}
    | Just (!bc, !bs) <- T.uncons bBegin
    , Just (!ec, !es) <- T.uncons bEnd =
        Just
            RegionBoundary
                { rbBegin = (# bc, bs #)
                , rbBeginLen = bBeginLen
                , rbEnd = (# ec, es #)
                , rbEndLen = bEndLen
                , rbEndUniform = bEndLen >= 3 && T.all (== ec) es
                , rbGuard = bGuard
                }
    | otherwise = Nothing

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
    let sortBounds = sortBy (comparing (Down . rbBeginLen)) . mapMaybe toRegionBoundary
        !cs' = sortBounds cs
        !ls' = sortBounds ls
        !rs' = sortBounds rs
        !chs' = sortBounds chs
        firstChar RegionBoundary{rbBegin = (# c, _ #)} = c
     in ParConfig
            { commBound = V.fromList cs'
            , litrBound = V.fromList ls'
            , rawBound = V.fromList rs'
            , chrBound = V.fromList chs'
            , inits =
                T.pack . Set.toList . Set.fromList $
                    (firstChar <$> cs')
                        <> (firstChar <$> ls')
                        <> (firstChar <$> rs')
                        <> (firstChar <$> chs')
            , useMakers = ab
            }

data SkipKind
    = NoSkip
    | SkipBlank
    | SkipDisplay !Bool
    | SkipToStartMarker !Bool
    deriving stock (Show, Eq)

data ParState = ParState
    { parCtxState :: !ContextState
    , parNextState :: !ContextState
    , parSkipKind :: !SkipKind
    , parSkip :: {-# UNPACK #-} !Int
    , parPrevChar :: {-# UNPACK #-} !Char
    , parEscaped :: !Bool
    , parText :: !T.Text
    }
    deriving stock (Show)

data ContextState
    = CodeState
    | CommState {-# UNPACK #-} !Int
    | ChrState {-# UNPACK #-} !Int
    | LitrState {-# UNPACK #-} !Int
    | RawState {-# UNPACK #-} !Int
    deriving stock (Show, Eq, Ord)

mkContextFilter :: Options -> ContextFilter
mkContextFilter Options{..} =
    if not (code || comment || literal)
        then contextFilterAll
        else ContextFilter $ contextBitCode ~= code .|. contextBitComment ~= comment .|. contextBitLiteral ~= literal

-- contextFilterFun:
--

blankChar :: Char -> Char
blankChar x = if isSpace x then x else blankByWidth x
{-# INLINE blankChar #-}

emitIf :: Bool -> Char -> Char
emitIf True x = x
emitIf False x = blankChar x
{-# INLINE emitIf #-}

runContextFilter :: ParConfig -> ContextFilter -> T.Text -> T.Text
runContextFilter conf f txt = T.unfoldr stepContext initialState
  where
    !codeDisp = codeFilter f
    !commDisp = commentFilter f
    !litrDisp = literalFilter f
    !markerDisp = codeDisp || litrDisp
    !markers = useMakers conf

    !initialState =
        ParState
            { parCtxState = CodeState
            , parNextState = CodeState
            , parSkipKind = NoSkip
            , parSkip = 0
            , parPrevChar = '\0'
            , parEscaped = False
            , parText = txt
            }

    stepContext :: ParState -> Maybe (Char, ParState)
    stepContext !s@ParState{..} = case T.uncons parText of
        Nothing -> Nothing
        Just (!c, !cont) ->
            let !(# !outChar, !nextState #) = stepChar s c cont
             in Just (outChar, nextState)
    {-# INLINE stepContext #-}

    stepChar :: ParState -> Char -> T.Text -> (# Char, ParState #)
    stepChar !s@ParState{..} !c !cont
        | parSkip > 0 =
            let !newSkip = parSkip - 1
                finishOrCont !outCh
                    | newSkip == 0 = (# outCh, ParState parNextState parNextState NoSkip 0 c False cont #)
                    | otherwise = (# outCh, s{parSkip = newSkip, parPrevChar = c, parEscaped = False, parText = cont} #)
             in case parSkipKind of
                    SkipDisplay disp -> finishOrCont (emitIf disp c)
                    SkipBlank -> finishOrCont (blankChar c)
                    SkipToStartMarker mDisp
                        | newSkip == 0 ->
                            let !outCh = if mDisp then startLiteralMarker else blankChar c
                             in (# outCh, ParState parNextState parNextState NoSkip 0 c False cont #)
                        | otherwise ->
                            (# blankChar c, s{parSkip = newSkip, parPrevChar = c, parEscaped = False, parText = cont} #)
                    NoSkip -> finishOrCont (blankChar c)
        | CodeState <- parCtxState = handleCodeState s c cont
        | CommState n <- parCtxState = handleCommentState s n c cont
        | LitrState n <- parCtxState = handleLiteralState s n c cont
        | ChrState n <- parCtxState = handleCharState s n c cont
        | RawState n <- parCtxState = handleRawState s n c cont
    {-# INLINE stepChar #-}

    {-# INLINE enterComment #-}
    enterComment !i !beginLen !c !cont =
        let !outCh = emitIf commDisp c
            !skip = beginLen - 1
            !target = CommState i
         in if skip == 0
                then (# outCh, ParState target target NoSkip 0 c False cont #)
                else (# outCh, ParState CodeState target (SkipDisplay commDisp) skip c False cont #)

    {-# INLINE enterLiteral #-}
    enterLiteral !target !beginLen !c !cont
        | markers =
            let !skip = beginLen - 1
             in if skip == 0
                    then
                        let !outCh = if markerDisp then startLiteralMarker else blankChar c
                         in (# outCh, ParState target target NoSkip 0 c False cont #)
                    else (# blankChar c, ParState CodeState target (SkipToStartMarker markerDisp) skip c False cont #)
        | otherwise =
            let !outCh = emitIf litrDisp c
                !skip = beginLen - 1
             in if skip == 0
                    then (# outCh, ParState target target NoSkip 0 c False cont #)
                    else (# outCh, ParState CodeState target (SkipDisplay litrDisp) skip c False cont #)

    {-# INLINE exitLiteral #-}
    exitLiteral !currSt !endLen !c !cont
        | markers =
            let !outCh = if markerDisp then endLiteralMarker else blankChar c
                !skip = endLen - 1
             in if skip == 0
                    then (# outCh, ParState CodeState CodeState NoSkip 0 c False cont #)
                    else (# outCh, ParState currSt CodeState SkipBlank skip c False cont #)
        | otherwise =
            let !outCh = emitIf litrDisp c
                !skip = endLen - 1
             in if skip == 0
                    then (# outCh, ParState CodeState CodeState NoSkip 0 c False cont #)
                    else (# outCh, ParState currSt CodeState (SkipDisplay litrDisp) skip c False cont #)

    {-# INLINE handleCodeState #-}
    handleCodeState !s@ParState{..} !c !cont =
        let !nextEsc = (c == '\\') && not parEscaped
         in if c `T.elem` inits conf
                then case findPrefixBoundary c cont parPrevChar parEscaped (commBound conf) of
                    (# i, Just !b #) -> enterComment i (rbBeginLen b) c cont
                    _ -> case findPrefixBoundary c cont parPrevChar parEscaped (rawBound conf) of
                        (# i, Just !b #) -> enterLiteral (RawState i) (rbBeginLen b) c cont
                        _ -> case findPrefixBoundary c cont parPrevChar parEscaped (litrBound conf) of
                            (# i, Just !b #) -> enterLiteral (LitrState i) (rbBeginLen b) c cont
                            _ -> case findPrefixBoundary' c cont parPrevChar parEscaped (chrBound conf) of
                                (# i, Just !b #) -> enterLiteral (ChrState i) (rbBeginLen b) c cont
                                _ -> (# emitIf codeDisp c, s{parPrevChar = c, parEscaped = nextEsc, parText = cont} #)
                else (# emitIf codeDisp c, s{parPrevChar = c, parEscaped = nextEsc, parText = cont} #)

    {-# INLINE handleCommentState #-}
    handleCommentState !s !n !c !cont =
        let !RegionBoundary{..} = V.unsafeIndex (commBound conf) n
            (# ec, es #) = rbEnd
            !outCh = emitIf commDisp c
         in if c == ec && es `T.isPrefixOf` cont
                then
                    let !skip = rbEndLen - 1
                     in if skip == 0
                            then (# outCh, ParState CodeState CodeState NoSkip 0 c False cont #)
                            else (# outCh, ParState (CommState n) CodeState (SkipDisplay commDisp) skip c False cont #)
                else (# outCh, s{parText = cont} #)

    {-# INLINE isClosingBoundary #-}
    isClosingBoundary !RegionBoundary{..} !c !cont =
        let (# ec, es #) = rbEnd
         in c == ec
                && es `T.isPrefixOf` cont
                && ( not rbEndUniform
                        || let !extra = T.length (T.takeWhile (== ec) (T.drop (rbEndLen - 1) cont))
                            in extra == 0 || extra >= rbEndLen
                   )

    {-# INLINE handleLiteralState #-}
    handleLiteralState !s !n !c !cont =
        if c == '\\'
            then (# emitIf litrDisp c, ParState (LitrState n) (LitrState n) (SkipDisplay litrDisp) 1 c False cont #)
            else
                let !rb@RegionBoundary{..} = V.unsafeIndex (litrBound conf) n
                 in if isClosingBoundary rb c cont
                        then exitLiteral (LitrState n) rbEndLen c cont
                        else (# emitIf litrDisp c, s{parText = cont} #)

    {-# INLINE handleCharState #-}
    handleCharState !s !n !c !cont =
        if c == '\\'
            then (# emitIf litrDisp c, ParState (ChrState n) (ChrState n) (SkipDisplay litrDisp) 1 c False cont #)
            else
                let !RegionBoundary{..} = V.unsafeIndex (chrBound conf) n
                    (# ec, es #) = rbEnd
                 in if c == ec && es `T.isPrefixOf` cont
                        then exitLiteral (ChrState n) rbEndLen c cont
                        else (# emitIf litrDisp c, s{parText = cont} #)

    {-# INLINE handleRawState #-}
    handleRawState !s !n !c !cont =
        let !rb@RegionBoundary{..} = V.unsafeIndex (rawBound conf) n
         in if isClosingBoundary rb c cont
                then exitLiteral (RawState n) rbEndLen c cont
                else (# emitIf litrDisp c, s{parText = cont} #)

isShellCommentDelimiter :: Char -> Bool
isShellCommentDelimiter ch =
    ch == '\0'
        || isSpace ch
        || ch == ';'
        || ch == '&'
        || ch == '|'
        || ch == '('
        || ch == ')'
        || ch == '<'
        || ch == '>'
{-# INLINE isShellCommentDelimiter #-}

checkBoundaryGuard :: BoundaryGuard -> Int -> Char -> Bool -> T.Text -> Bool
checkBoundaryGuard NoBoundaryGuard _ _ _ _ = True
checkBoundaryGuard GuardShellComment _ prevCh prevEsc _ =
    not prevEsc && isShellCommentDelimiter prevCh
checkBoundaryGuard GuardNotPrecededBySlash _ _ prevEsc _ =
    not prevEsc
checkBoundaryGuard (GuardNotFollowedBy forbidden) beginLen _ _ xs =
    case T.uncons (T.drop (beginLen - 1) xs) of
        Just (!nc, _) -> nc /= forbidden
        Nothing -> True
{-# INLINE checkBoundaryGuard #-}

findPrefixBoundary :: Char -> T.Text -> Char -> Bool -> V.Vector RegionBoundary -> (# Int, Maybe RegionBoundary #)
findPrefixBoundary !x !xs !prevCh !prevEsc !vb = go 0
  where
    !len = V.length vb
    go !i
        | i >= len = (# 0, Nothing #)
        | otherwise =
            let !rb@RegionBoundary{..} = V.unsafeIndex vb i
                (# !c, !cont #) = rbBegin
             in if c == x
                    then
                        if cont `T.isPrefixOf` xs && checkBoundaryGuard rbGuard rbBeginLen prevCh prevEsc xs
                            then (# i, Just rb #)
                            else go (i + 1)
                    else go (i + 1)
{-# INLINE findPrefixBoundary #-}

findPrefixBoundary' :: Char -> T.Text -> Char -> Bool -> V.Vector RegionBoundary -> (# Int, Maybe RegionBoundary #)
findPrefixBoundary' !x !xs !prevCh !prevEsc !vb = go 0
  where
    !len = V.length vb
    go !i
        | i >= len = (# 0, Nothing #)
        | otherwise =
            let !rb@RegionBoundary{..} = V.unsafeIndex vb i
                (# !c, !cont #) = rbBegin
             in if c /= x || not (cont `T.isPrefixOf` xs) || not (checkBoundaryGuard rbGuard rbBeginLen prevCh prevEsc xs)
                    then go (i + 1)
                    else
                        if hasMatchingCharEnd rb (T.drop (rbBeginLen - 1) xs)
                            then (# i, Just rb #)
                            else go (i + 1)

    hasMatchingCharEnd :: RegionBoundary -> T.Text -> Bool
    hasMatchingCharEnd RegionBoundary{rbEnd = (# !e, !es #)} body =
        let matchEnd t = case T.uncons t of
                Just (!z, !zs) -> z == e && es `T.isPrefixOf` zs
                Nothing -> False
            scanEsc :: Int -> T.Text -> Bool
            scanEsc !remSteps t
                | matchEnd t = True
                | remSteps <= 0 = False
                | Just (!ch, !rest) <- T.uncons t
                , isAlphaNum ch || ch == '{' || ch == '}' =
                    scanEsc (remSteps - 1) rest
                | otherwise = False
         in case T.uncons body of
                Just ('\\', afterSlash) ->
                    case T.uncons afterSlash of
                        Just (!esc1, !rest1)
                            | esc1 /= '\n' && esc1 /= '\r' -> scanEsc 10 rest1
                        _ -> False
                Just (!y, !ys)
                    | y /= '\n' && y /= '\r' && y /= e -> matchEnd ys
                _ -> False

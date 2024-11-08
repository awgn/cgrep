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

module CGrep.ContextFilter where

import CGrep.Parser.Char (chr, isSpace, ord)
import CGrep.Types (Text8)

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Short as B

import qualified Data.Map as Map

import CGrep.Boundary (Boundary (..))
import qualified Data.Aeson.KeyMap as B
import Data.List (find, findIndex, nub)
import Data.Maybe (fromMaybe, isJust)
import Options (Options (..))

import Data.Bits (Bits (complement, shiftL, shiftR, xor, (.&.), (.|.)))
import Data.Int (Int32, Int64)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import qualified Data.ByteString.Unsafe as U
import Data.HashMap.Internal.Strict (alter)
import Data.Word (Word64)

import Util (findWithIndex)

type FilterFunction = ContextFilter -> Text8 -> Text8

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

data ParConfig = ParConfig
    { commBound :: [Boundary]
    , litrBound :: [Boundary]
    , rawBound :: [Boundary]
    , chrBound :: [Boundary]
    , inits :: B.ShortByteString
    , alterBoundary :: Bool
    }

mkParConfig :: [Boundary] -> [Boundary] -> [Boundary] -> [Boundary] -> Bool -> ParConfig
mkParConfig cs ls rs chs ab =
    ParConfig
        { commBound = cs
        , litrBound = ls
        , rawBound = rs
        , chrBound = chs
        , inits =
            (B.pack . nub)
                ( (fromIntegral . ord . C.head . bBegin <$> cs)
                    <> (fromIntegral . ord . C.head . bBegin <$> ls)
                    <> (fromIntegral . ord . C.head . bBegin <$> rs)
                    <> (fromIntegral . ord . C.head . bBegin <$> chs)
                )
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

unpackBoundary :: Boundary -> (String, String)
unpackBoundary (Boundary a b) = (C.unpack a, C.unpack b)
{-# INLINE unpackBoundary #-}

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

data ParData = ParData
    { pdText :: {-# UNPACK #-} !Text8
    , pdState :: !ParState
    }

runContextFilter :: ParConfig -> ContextFilter -> Text8 -> Text8
runContextFilter conf@ParConfig{..} f txt
    | alterBoundary = fst $ C.unfoldrN (C.length txt) (contextFilter' conf) (ParData txt (ParState CodeState1 CodeState1 (codeFilter f) 0))
    | otherwise = fst $ C.unfoldrN (C.length txt) (contextFilter'' conf) (ParData txt (ParState CodeState1 CodeState1 (codeFilter f) 0))
  where
    contextFilter' :: ParConfig -> ParData -> Maybe (Char, ParData)
    contextFilter' c (ParData txt@(C.uncons -> Just (x, xs)) s) =
        let !s' = nextContextState c s txt f
         in if display s'
                then case (# getContext (ctxState s), getContext (ctxState s') #) of
                    (# Code, Literal #) -> Just (chr 2, ParData xs s')
                    (# Literal, Code #) -> Just (chr 3, ParData xs s')
                    _ -> Just (x, ParData xs s')
                else
                    if isSpace x
                        then Just (x, ParData xs s')
                        else Just (' ', ParData xs s')
    contextFilter' _ (ParData (C.uncons -> Nothing) _) = Nothing

    contextFilter'' :: ParConfig -> ParData -> Maybe (Char, ParData)
    contextFilter'' c (ParData txt@(C.uncons -> Just (x, xs)) s) =
        let !s' = nextContextState c s txt f
         in if display s' || isSpace x
                then Just (x, ParData xs s')
                else Just (' ', ParData xs s')

{-# INLINE nextContextState #-}
nextContextState :: ParConfig -> ParState -> Text8 -> ContextFilter -> ParState
nextContextState c s@ParState{..} txt f
    | skip > 0 = {-# SCC skip #-} transState s{skip = skip - 1}
    | CodeState1 <- ctxState =
        {-# SCC next_code1 #-}
        if U.unsafeHead txt `B.elem` inits c
            then case findPrefixBoundary txt (commBound c) of
                (# i, Just b #) -> {-# SCC next_code1_1 #-} transState s{nextState = CommState1 i, display = commentFilter f, skip = C.length (bBegin b) - 1}
                _ -> case findPrefixBoundary txt (litrBound c) of
                    (# i, Just b #) -> {-# SCC next_code1_2 #-} transState s{nextState = LitrState1 i, display = codeFilter f, skip = C.length (bBegin b) - 1}
                    _ -> case findPrefixBoundary txt (rawBound c) of
                        (# i, Just b #) -> {-# SCC next_code1_3 #-} transState s{nextState = RawState i, display = codeFilter f, skip = C.length (bBegin b) - 1}
                        _ -> case findPrefixBoundary' txt (chrBound c) of
                            (# i, Just b #) -> transState s{nextState = ChrState i, display = codeFilter f, skip = C.length (bBegin b) - 1}
                            _ -> {-# SCC next_code1_5 #-} s{ctxState = CodeStateN, nextState = CodeStateN, display = codeFilter f, skip = 0}
            else {-# SCC next_code1_0 #-} s{ctxState = CodeStateN, nextState = CodeStateN, display = codeFilter f, skip = 0}
    | CodeStateN <- ctxState =
        {-# SCC next_code #-}
        if {-# SCC next_code_if #-} U.unsafeHead txt `B.elem` inits c
            then
                {-# SCC next_code_then #-}
                case findPrefixBoundary txt (commBound c) of
                    (# i, Just b #) -> {-# SCC next_code1_1 #-} transState s{nextState = CommState1 i, display = commentFilter f, skip = C.length (bBegin b) - 1}
                    _ -> case findPrefixBoundary txt (litrBound c) of
                        (# i, Just b #) -> {-# SCC next_code1_2 #-} transState s{nextState = LitrState1 i, display = codeFilter f, skip = C.length (bBegin b) - 1}
                        _ -> case findPrefixBoundary txt (rawBound c) of
                            (# i, Just b #) -> {-# SCC next_code1_3 #-} transState s{nextState = RawState i, display = codeFilter f, skip = C.length (bBegin b) - 1}
                            _ -> case findPrefixBoundary' txt (chrBound c) of
                                (# i, Just b #) -> transState s{nextState = ChrState i, display = codeFilter f, skip = C.length (bBegin b) - 1}
                                _ -> {-# SCC next_code_5 #-} s
            else {-# SCC next_code_else #-} s
    | CommState1 n <- ctxState =
        let Boundary _ e = commBound c !! n
         in {-# SCC next_comm1 #-}
            if e `C.isPrefixOf` txt
                then transState $ s{nextState = CodeState1, display = commentFilter f, skip = C.length e - 1}
                else s{ctxState = CommStateN n, nextState = CommStateN n, display = commentFilter f, skip = 0}
    | CommStateN n <- ctxState =
        let Boundary _ e = commBound c !! n
         in {-# SCC next_comm #-}
            if e `C.isPrefixOf` txt
                then transState $ s{nextState = CodeState1, display = commentFilter f, skip = C.length e - 1}
                else s
    | LitrState1 n <- ctxState =
        if C.head txt == '\\'
            then s{display = displayContext ctxState f, skip = 1}
            else
                let Boundary _ e = litrBound c !! n
                 in {-# SCC next_liter #-}
                    if e `C.isPrefixOf` txt
                        then s{ctxState = CodeState1, nextState = CodeState1, display = codeFilter f, skip = C.length e - 1}
                        else s{ctxState = LitrStateN n, nextState = LitrStateN n, display = literalFilter f, skip = 0}
    | LitrStateN n <- ctxState =
        if C.head txt == '\\'
            then s{display = displayContext ctxState f, skip = 1}
            else
                let Boundary _ e = litrBound c !! n
                 in {-# SCC next_liter #-}
                    if e `C.isPrefixOf` txt
                        then s{ctxState = CodeState1, nextState = CodeState1, display = codeFilter f, skip = C.length e - 1}
                        else s
    | ChrState n <- ctxState =
        if C.head txt == '\\'
            then s{display = displayContext ctxState f, skip = 1}
            else
                let Boundary _ e = chrBound c !! n
                 in {-# SCC next_chr #-}
                    if e `C.isPrefixOf` txt
                        then s{ctxState = CodeState1, nextState = CodeState1, display = codeFilter f, skip = C.length e - 1}
                        else s{display = literalFilter f, skip = 0}
    | RawState n <- ctxState =
        let Boundary _ e = rawBound c !! n
         in {-# SCC next_raw #-}
            if e `C.isPrefixOf` txt
                then s{ctxState = CodeState1, nextState = CodeState1, display = codeFilter f, skip = C.length e - 1}
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

findPrefixBoundary :: Text8 -> [Boundary] -> (# Int, Maybe Boundary #)
findPrefixBoundary xs vb =
    {-# SCC findPrefixBoundary #-}
    findWithIndex (\(Boundary b _) -> b `C.isPrefixOf` xs) vb
{-# INLINE findPrefixBoundary #-}

findPrefixBoundary' :: Text8 -> [Boundary] -> (# Int, Maybe Boundary #)
findPrefixBoundary' txt bs =
    case findWithIndex (\(Boundary beg _) -> beg `C.isPrefixOf` txt) bs of
        elm@(# idx, Just b@(Boundary _ end) #) -> case C.tail txt of
            (C.uncons -> Just (y, ys)) ->
                let skip = if y == '\\' then 1 else 0
                 in if end `C.isPrefixOf` C.drop skip ys then elm else (# 0, Nothing #)
            _ -> (# 0, Nothing #)
        _ -> (# 0, Nothing #)

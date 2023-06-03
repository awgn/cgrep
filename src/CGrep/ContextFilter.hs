
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

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CGrep.ContextFilter where

import CGrep.Types ( Text8 )

import Data.Char ( isSpace, chr, ord )

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Short as B

import qualified Data.Map as Map

import Options ( Options(..) )
import Data.List (findIndex, nub)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Aeson.KeyMap as B
import CGrep.Boundary ( Boundary(..) )

import Data.Int ( Int32, Int64 )
import Data.Bits ( Bits((.|.), complement, (.&.), xor, shiftL, shiftR) )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Unsafe.Coerce
import Data.HashMap.Internal.Strict (alter)
import Data.Word (Word64)
import Debug.Trace

type FilterFunction = ContextFilter -> Text8 -> Text8

data Context = Code | Comment | Literal
    deriving (Eq, Show)

newtype ContextBit = ContextBit Int32
    deriving stock (Eq, Show)
    deriving newtype (Bits)


contextBitEmpty :: ContextBit
contextBitEmpty   = ContextBit 0
contextBitCode :: ContextBit
contextBitCode    = ContextBit 0x1
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


newtype ContextFilter = ContextFilter { unFilter :: ContextBit }
    deriving stock (Eq, Show)
    deriving newtype (Bits)

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


data ParConfig  =  ParConfig
    {   commBound     :: V.Vector Boundary
    ,   litrBound     :: V.Vector Boundary
    ,   rawBound      :: V.Vector Boundary
    ,   chrBound      :: V.Vector Boundary
    ,   inits         :: B.ShortByteString
    ,   alterBoundary :: Bool
    }


mkParConfig :: [Boundary] -> [Boundary] -> [Boundary] -> [Boundary] -> Bool -> ParConfig
mkParConfig cs ls rs chs ab =
    ParConfig {
        commBound = V.fromList cs
    ,   litrBound = V.fromList ls
    ,   rawBound  = V.fromList rs
    ,   chrBound  = V.fromList chs
    ,   inits     = (B.pack . nub) ((fromIntegral . ord . C.head . bBegin <$> cs) <>
                                    (fromIntegral . ord . C.head . bBegin <$> ls) <>
                                    (fromIntegral . ord . C.head . bBegin <$> rs) <>
                                    (fromIntegral . ord . C.head . bBegin <$> chs))
    ,   alterBoundary = ab }

data ParState =  ParState
    {   ctxState  :: !ContextState
    ,   nextState :: !ContextState
    ,   display   :: !Bool
    ,   skip      :: {-# UNPACK #-} !Int
    } deriving (Show)


data ContextState = CodeState | CommState {-# UNPACK #-} !Int | ChrState {-# UNPACK #-} !Int | LitrState {-# UNPACK #-} !Int | RawState {-# UNPACK #-} !Int
    deriving (Show, Eq, Ord)


mkContextFilter :: Options -> ContextFilter
mkContextFilter Options{..} =
    if not (code || comment || literal)
        then contextFilterAll
        else ContextFilter $ contextBitCode ~= code .|. contextBitComment ~= comment .|. contextBitLiteral ~= literal


unpackBoundary :: Boundary -> (String, String)
unpackBoundary (Boundary a b) = (C.unpack a, C.unpack b)
{-# INLINE unpackBoundary #-}


getContext :: ContextState -> Context
getContext CodeState = Code
getContext (CommState _ ) = Comment
getContext (LitrState _)  = Literal
getContext (RawState _)   = Literal
getContext (ChrState _)   = Literal
{-# INLINE  getContext #-}

-- contextFilterFun:
--

type ParData = ( Text8, ParState )

runContextFilter :: ParConfig -> ContextFilter -> Text8 -> Text8
runContextFilter conf@ParConfig{..} f txt | alterBoundary = fst $ C.unfoldrN (C.length txt) (contextFilter' conf) ( txt, ParState CodeState CodeState (codeFilter f) 0 )
                                             | otherwise     = fst $ C.unfoldrN (C.length txt) (contextFilter'' conf) ( txt, ParState CodeState CodeState (codeFilter f) 0 )
    where contextFilter' :: ParConfig -> ParData ->  Maybe (Char, ParData)
          contextFilter' c (txt@(C.uncons -> Just (x,xs)), s) = Just (x', (xs', s'))
              where s' = nextContextState c s txt f
                    (# x', xs' #)   | display s' = case (# ctxState s, ctxState s' #) of
                                                    (# getContext -> Code, getContext -> Literal #) -> (# chr 2, xs #)
                                                    (# getContext -> Literal, getContext -> Code #) -> (# chr 3, xs #)
                                                    _                                               -> (# x , xs #)
                                    | isSpace x  = (# x , xs #)
                                    | otherwise  = (#' ', xs #)
          contextFilter' _ (C.uncons -> Nothing, _) = Nothing

          contextFilter'' :: ParConfig -> ParData ->  Maybe (Char, ParData)
          contextFilter'' c (txt@(C.uncons -> Just (x,xs)), s) = Just (x', (xs', s'))
              where s' = nextContextState c s txt f
                    (# x', xs' #)   | display s' = (# x, xs #)
                                    | isSpace x  = (# x , xs #)
                                    | otherwise  = (#' ', xs #)
          contextFilterImpl _ (C.uncons -> Nothing, _) = Nothing


displayContext :: ContextState -> ContextFilter -> Bool
displayContext  CodeState     cf = cf ~? contextBitCode
displayContext  (CommState _) cf = cf ~? contextBitComment
displayContext  (LitrState _) cf = cf ~? contextBitLiteral
displayContext  (RawState _)  cf = cf ~? contextBitLiteral
displayContext  (ChrState _)  cf = cf ~? contextBitLiteral
{-# INLINE displayContext #-}


transState :: ParState -> ParState
transState s@ParState {..} | skip == 0 = s{ ctxState = nextState }
                           | otherwise = s
{-# INLINE transState #-}


nextContextState :: ParConfig -> ParState -> Text8 -> ContextFilter -> ParState
nextContextState c s@ParState{..} txt f
    | skip > 0   = {-# SCC skip #-} transState s{ skip = skip - 1 }

    |  CodeState  <- ctxState = {-# SCC next_code #-} if (fromIntegral . ord) (C.head txt) `B.elem` inits c
        then if | Just i <-    {-# SCC next_code_1 #-} findPrefixBoundary  txt (commBound c) -> transState s{ nextState = CommState i, display = commentFilter f, skip = C.length (bBegin (commBound c `V.unsafeIndex` i)) - 1 }
                | Just i <-    {-# SCC next_code_2 #-} findPrefixBoundary  txt (litrBound c) -> transState s{ nextState = LitrState i, display = codeFilter f,    skip = C.length (bBegin (litrBound c `V.unsafeIndex` i)) - 1 }
                | Just i <-    {-# SCC next_code_3 #-} findPrefixBoundary  txt (rawBound  c) -> transState s{ nextState = RawState  i, display = codeFilter f,    skip = C.length (bBegin (rawBound c  `V.unsafeIndex` i)) - 1 }
                | Just i <-    {-# SCC next_code_4 #-} findPrefixBoundary' txt (chrBound  c) -> transState s{ nextState = ChrState  i, display = codeFilter f, skip = C.length (bBegin (chrBound c  `V.unsafeIndex` i)) - 1 }
                | otherwise -> {-# SCC next_code_5 #-} s{ display = codeFilter f, skip = 0 }
        else {-# SCC next_code_0 #-} s{ display = codeFilter f, skip = 0 }

    | CommState n <- ctxState =
        let Boundary _ e = commBound c `V.unsafeIndex` n
            in {-# SCC next_comm #-} if e `C.isPrefixOf` txt
                then transState $ s{ nextState = CodeState, display = commentFilter f, skip = C.length e - 1}
                else s { display = commentFilter f, skip = 0 }

    | LitrState n <- ctxState  =
        if C.head txt == '\\'
        then s { display = displayContext ctxState f, skip = 1 }
        else let Boundary _ e = litrBound c `V.unsafeIndex` n
               in {-# SCC next_liter #-} if e `C.isPrefixOf` txt
                    then s{ ctxState = CodeState, nextState = CodeState, display = codeFilter f, skip = C.length e - 1}
                    else s{ display = literalFilter f, skip = 0 }

    | ChrState n <- ctxState  =
        if C.head txt == '\\'
        then s { display = displayContext ctxState f, skip = 1 }
        else let Boundary _ e = chrBound c `V.unsafeIndex` n
                in {-# SCC next_chr #-} if e `C.isPrefixOf` txt
                    then s{ ctxState = CodeState, nextState = CodeState, display = codeFilter f, skip = C.length e - 1}
                    else s{ display = literalFilter f , skip = 0}

    | RawState n <- ctxState  =
        let Boundary _ e = rawBound c `V.unsafeIndex` n
            in {-# SCC next_raw #-} if e `C.isPrefixOf` txt
                then s{ ctxState = CodeState, nextState = CodeState, display = codeFilter f, skip = C.length e - 1}
                else s{ display = literalFilter f , skip = 0}


findPrefixBoundary :: Text8 -> V.Vector Boundary -> Maybe Int
findPrefixBoundary xs = {-# SCC findPrefixBoundary #-}  V.findIndex (\(Boundary b _ ) -> b `C.isPrefixOf` xs)
{-# INLINE findPrefixBoundary #-}


findPrefixBoundary' :: Text8 -> V.Vector Boundary -> Maybe Int
findPrefixBoundary' txt bs =
    case V.findIndex (\(Boundary b _ ) -> b `C.isPrefixOf` txt) bs of
        (Just i) -> let (Boundary _ e) = bs `V.unsafeIndex` i
                    in case C.tail txt of
                        (C.uncons -> Just (y , ys)) ->
                            let skip = if y == '\\' then 1 else 0
                                in if e `C.isPrefixOf` C.drop skip ys then Just i else Nothing
                        _ -> Nothing
        _ -> Nothing
{-# INLINEABLE findPrefixBoundary' #-}

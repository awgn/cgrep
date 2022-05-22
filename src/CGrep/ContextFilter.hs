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

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module CGrep.ContextFilter where

import CGrep.Types ( Text8 )

import Data.Char ( isSpace, chr )

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map
import qualified Data.Array.BitArray as BA
import Data.Array.BitArray ((!))

import GHC.Prim ( (+#) )
import GHC.Exts ( Int(I#), (+#) )

import Options ( Options(..) )
import Data.List (findIndex)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Aeson.KeyMap as B

import CGrep.Boundary ( Boundary(..) )

type FilterFunction = ContextFilter -> Text8 -> Text8


data Context = Code | Comment | Literal
    deriving (Eq, Show)


data ContextFilter = ContextFilter
    {   ctxCode    :: !Bool
    ,   ctxComment :: !Bool
    ,   ctxLiteral :: !Bool
    } deriving (Eq, Show)


data ParConfig  =  ParConfig
    {   commBound     :: [Boundary]
    ,   litrBound     :: [Boundary]
    ,   rawBound      :: [Boundary]
    ,   chrBound      :: [Boundary]
    ,   bloom         :: BA.BitArray Char
    ,   alterBoundary :: Bool
    }


mkParConfig :: [Boundary] -> [Boundary] -> [Boundary] -> [Boundary] -> Bool -> ParConfig
mkParConfig cs ls rs chs ab =
    ParConfig {
        commBound = cs
    ,   litrBound = ls
    ,   rawBound = rs
    ,   chrBound = chs
    ,   bloom = mkBoundaryBloom (unpackBoundary <$> (cs <> ls <> rs <> chs))
    ,   alterBoundary = ab }

    where mkBoundaryBloom :: [(String, String)] -> BA.BitArray Char
          mkBoundaryBloom bs = BA.listArray ('\0', '\255') (map (\c -> isJust (findIndex (\(b,_) -> c == head b) bs)) ['\0'..'\255'])


data ParState =  ParState
    {   ctxState  :: !ContextState
    ,   nextState :: !ContextState
    ,   display   :: !Bool
    ,   skip      :: {-# UNPACK #-} !Int
    } deriving (Show)


data ContextState = CodeState | CommState {-# UNPACK #-} !Int | ChrState {-# UNPACK #-} !Int | LitrState {-# UNPACK #-} !Int | RawState {-# UNPACK #-} !Int
    deriving (Show, Eq, Ord)


type ParData = (Text8, ContextFilter, ParState)


mkContextFilter :: Options -> ContextFilter
mkContextFilter Options{..} =
    if not (code || comment || literal)
        then ContextFilter { ctxCode = True, ctxComment = True,  ctxLiteral = True }
        else ContextFilter { ctxCode = code , ctxComment = comment , ctxLiteral = literal }



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

runContextFilter :: ParConfig -> ContextFilter -> Text8 -> Text8
runContextFilter conf@ParConfig{..} filt txt =
  fst $ C.unfoldrN (C.length txt) (contextFilterImpl' conf) (txt, filt, ParState CodeState CodeState False 0)
    where contextFilterImpl' :: ParConfig -> ParData ->  Maybe (Char, ParData)
          contextFilterImpl' _ (C.uncons -> Nothing, _, _)     = Nothing
          contextFilterImpl' c (C.uncons -> Just (x,xs), f, s) = Just (x', (xs', f, s'))
              where !s' = nextContextState c s (x,xs) f
                    !(x', xs') | display s' = if alterBoundary
                                                then case (ctxState s, ctxState s') of
                                                        (getContext -> Code, getContext -> Literal) -> (chr 2, xs)
                                                        (getContext -> Literal, getContext -> Code) -> (chr 3, xs)
                                                        _                                           -> (x , xs)
                                                else (x, xs)
                               | isSpace x  = (x  , xs)
                               | otherwise  = (' ', xs)

          contextFilterImpl' _ _ = undefined


{-# INLINE displayContext #-}
displayContext :: ContextState -> ContextFilter -> Bool
displayContext  CodeState     (ContextFilter b _ _ ) = b
displayContext  (CommState _) (ContextFilter _ b _ ) = b
displayContext  (LitrState _) (ContextFilter _ _ b ) = b
displayContext  (RawState _)  (ContextFilter _ _ b ) = b
displayContext  (ChrState _)  (ContextFilter _ _ b ) = b


transState :: ParState -> ParState
transState s@ParState {..} = if skip == 0
                                then s{ ctxState = nextState }
                                else s
{-# INLINE transState #-}


nextContextState :: ParConfig -> ParState -> (Char,Text8) -> ContextFilter -> ParState
nextContextState c s@ParState{..} (x,xs) filt@(ContextFilter codefilt commfilt litrfilt)
    | skip > 0   = transState $ s { skip = skip - 1 }

    | CodeState  <- ctxState =
        let cindex = findPrefixBoundary (x,xs) (commBound c)
            lindex = findPrefixBoundary (x,xs) (litrBound c)
            rindex = findPrefixBoundary (x,xs) (rawBound  c)
            hindex = findPrefixBoundary' (x,xs) (chrBound  c)
          in if bloom c ! x
                then if | Just i <- cindex -> transState $ s{ nextState = CommState i, display = commfilt, skip = C.length (bBegin (commBound c !! i)) - 1 }
                        | Just i <- lindex -> transState $ s{ nextState = LitrState i, display = codefilt, skip = C.length (bBegin (litrBound c !! i)) - 1 }
                        | Just i <- rindex -> transState $ s{ nextState = RawState i,  display = codefilt, skip = C.length (bBegin (rawBound c  !! i)) - 1 }
                        | Just i <- hindex -> transState $ s{ nextState = ChrState i,  display = codefilt, skip = C.length (bBegin (chrBound c  !! i)) - 1 }
                        | otherwise        -> s{ display = codefilt, skip = 0 }
                else s{ display = codefilt, skip = 0 }

    | CommState n <- ctxState =
        let Boundary _ e = commBound c !! n
            in if C.head e == x && C.tail e `C.isPrefixOf` xs
                then transState $ s{ nextState = CodeState, display =  commfilt, skip = C.length e - 1}
                else s{ display = commfilt, skip = 0 }

    | LitrState n <- ctxState  =
        if x == '\\'
        then s { display = displayContext ctxState filt, skip = 1 }
        else let Boundary _ e = litrBound c !! n
               in if C.head e == x && C.tail e `C.isPrefixOf` xs
                    then s{ ctxState = CodeState, nextState = CodeState, display = codefilt, skip = C.length e - 1}
                    else s{ display = litrfilt, skip = 0 }
    | ChrState n <- ctxState  =
        if x == '\\'
        then s { display = displayContext ctxState filt, skip = 1 }
        else let Boundary _ e = chrBound c !! n
                in if C.head e == x && C.tail e `C.isPrefixOf` xs
                    then s{ ctxState = CodeState, nextState = CodeState, display = codefilt, skip = C.length e - 1}
                    else s{ display = litrfilt, skip = 0 }
    | RawState n <- ctxState  =
        let Boundary _ e = rawBound c !! n
            in if C.head e == x && C.tail e `C.isPrefixOf` xs
                then s{ ctxState = CodeState, nextState = CodeState, display = codefilt, skip = C.length e - 1}
                else s{ display = litrfilt, skip = 0 }


findPrefixBoundary :: (Char, Text8) -> [Boundary] -> Maybe Int
findPrefixBoundary (x,xs) =  findIndex (\(Boundary b _ ) -> C.head b == x && C.tail b `C.isPrefixOf` xs)
{-# INLINE findPrefixBoundary #-}


findPrefixBoundary' :: (Char, Text8) -> [Boundary] -> Maybe Int
findPrefixBoundary' (x,xs) bs =
    case findIndex (\(Boundary b _ ) -> C.head b == x && C.tail b `C.isPrefixOf` xs) bs of
        (Just i) -> let (Boundary _ e) = bs !! i
                    in case xs of
                        (C.uncons -> Just (y , ys)) ->
                            let skip = if y == '\\' then 1 else 0
                                in if e `C.isPrefixOf` C.drop skip ys then Just i else Nothing
                        _ -> Nothing
        _ -> Nothing

{-# INLINE findPrefixBoundary' #-}

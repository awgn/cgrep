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

module CGrep.ContextFilter where

import CGrep.Types ( Text8 )

import Data.Char ( isSpace )

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map
import qualified Data.Array.BitArray as BA
import Data.Array.BitArray ((!))

import GHC.Prim ( (+#) )
import GHC.Exts ( Int(I#), (+#) )

import Options ( Options(..) )

type FilterFunction = ContextFilter -> Text8 -> Text8

data Context = Code | Comment | Literal
    deriving (Eq, Show)


data ContextFilter = ContextFilter
    {   ctxCode    :: !Bool
    ,   ctxComment :: !Bool
    ,   ctxLiteral :: !Bool
    } deriving (Eq, Show)


data Boundary = Boundary
    {   _beg   :: !Text8
    ,   _end   :: !Text8
    } deriving (Show)


data ParConf  =  ParConf
    {   commBound :: [Boundary]
    ,   litrBound :: [Boundary]
    ,   bloom     :: BA.BitArray Char
    }


data ParState =  ParState
    {   cxtState  :: !ContextState
    ,   display   :: !Bool
    ,   skip      :: {-# UNPACK #-} !Int
    } deriving (Show)


data ContextState = CodeState | CommState {-# UNPACK #-} !Int | LitrState {-# UNPACK #-} !Int
    deriving (Show, Eq, Ord)


type ParData = (Text8, ContextFilter, ParState)


-- contextFilterFun:
--

contextFilterFun :: ParConf -> ContextFilter -> Text8 -> Text8
contextFilterFun conf filt txt =
  fst $ C.unfoldrN (C.length txt) (contextFilterImpl conf) (txt, filt, ParState CodeState False 0)
{-# INLINE contextFilterFun #-}


contextFilterImpl :: ParConf -> ParData ->  Maybe (Char, ParData)
contextFilterImpl _ (C.uncons -> Nothing, _, _)     = Nothing
contextFilterImpl c (C.uncons -> Just (x,xs), f, s) = Just (c', (xs, f, s'))
    where !s' = nextContextState c s (x,xs) f
          !c' = if display s' || isSpace x then x else ' '
contextFilterImpl _ _ = undefined


{-# INLINE displayContext #-}
displayContext :: ContextState -> ContextFilter -> Bool
displayContext  CodeState     (ContextFilter b _ _ ) = b
displayContext  (CommState _) (ContextFilter _ b _ ) = b
displayContext  (LitrState _) (ContextFilter _ _ b ) = b


nextContextState :: ParConf -> ParState -> (Char,Text8) -> ContextFilter -> ParState
nextContextState c s (x,xs) filt@(ContextFilter codefilt commfilt litrfilt)
    | skip s > 0                           = s { skip = skip s - 1 }
    | x == '\'' && "\"'" `C.isPrefixOf` xs = s { skip = 2 }
    | x == '\\'                            = s { display = displayContext (cxtState s) filt, skip = 1 }

    | CodeState   <- cxtState s = let cindex = findBoundary (x,xs) (commBound c)
                                      lindex = findBoundary (x,xs) (litrBound c)
                                  in if bloom c ! x
                                     then if cindex >= 0
                                          then s{ cxtState = CommState cindex, display = commfilt, skip = C.length ( _beg (commBound c !! cindex) ) - 1 }
                                          else if lindex >= 0
                                               then s{ cxtState = LitrState lindex, display = codefilt, skip = C.length ( _beg (litrBound c !! lindex) ) - 1 }
                                               else s{ display  = codefilt, skip = 0 }
                                     else s{ display  = codefilt, skip = 0 }

    | CommState n <- cxtState s = let Boundary _ e = commBound c !! n
                                  in if C.head e == x && C.tail e `C.isPrefixOf` xs
                                     then s{ cxtState = CodeState, display = commfilt, skip = C.length e - 1}
                                     else s{ display  = commfilt, skip = 0 }

    | LitrState n <- cxtState s = let Boundary _ e = litrBound c !! n
                                  in if C.head e == x && C.tail e `C.isPrefixOf` xs
                                     then s{ cxtState = CodeState, display = codefilt, skip = C.length e - 1}
                                     else s{ display = litrfilt, skip = 0 }

findBoundary :: (Char, Text8) -> [Boundary] -> Int
findBoundary (x,xs) =  findIndex' (\(Boundary b _ ) -> C.head b == x && C.tail b `C.isPrefixOf` xs)
{-# INLINE findBoundary #-}


mkContextFilter :: Options -> ContextFilter
mkContextFilter Options{..} =
    if not (code || comment || literal)
        then ContextFilter { ctxCode = True, ctxComment = True,  ctxLiteral = True }
        else ContextFilter { ctxCode = code , ctxComment = comment , ctxLiteral = literal }


findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' p ls =
    loop 0# ls
        where loop _ [] = -1
              loop n (x:xs) | p x       = I# n
                            | otherwise = loop (n +# 1#) xs

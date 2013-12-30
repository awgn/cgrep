--
-- Copyright (c) 2013 Bonelli Nicola <bonelli@antifork.org>
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

{-# LANGUAGE TupleSections #-} 

module CGrep.Strategy.Search (search) where

import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Search as SC

import CGrep.Common
import CGrep.Filter 
import CGrep.Lang

import qualified CGrep.Token as T

-- import Control.Monad

import Options 
import Debug

import Control.Arrow as A

search :: CgrepFunction
search opt ps f = do

    let filename = getFileName f 
     
    text <- getText f
    
    -- transform text
    
    let text' = ignoreCase opt . expandMultiline opt . contextFilter (getLang opt filename) (mkContextFilter opt) $ text

    -- search for matching tokens
    
    let tokens  = map (A.second C.unpack) $ ps >>= (\p -> map (\i -> (i,p)) (p `SC.nonOverlappingIndices` text')) 

    -- filter exact matching tokens

    let tokens' = if word_match opt then filter (T.isCompleteToken text') tokens 
                                    else tokens

    putStrLevel1 (debug opt) $ "strategy  : running string search on " ++ filename ++ "..."
    putStrLevel2 (debug opt) $ "tokens    : " ++ show tokens'
    putStrLevel3 (debug opt) $ "---\n" ++ C.unpack text' ++ "\n---"

    return $ mkOutput opt filename text tokens'  


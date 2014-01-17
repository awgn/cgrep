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

module CGrep.Strategy.Cpp.Semantic (search) where

import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M

import CGrep.WildCard
import CGrep.Filter 
import CGrep.Lang
import CGrep.Common
import CGrep.Output
import CGrep.Distance

import Data.Char
import Data.List
import Data.Function

import Options 
import Debug


import qualified CGrep.Strategy.Cpp.Token  as Cpp


search :: CgrepFunction
search opt ps f = do
    
    let filename = getFileName f 
    
    text <- getText f 

    -- transform text
    
    let text' = ignoreCase opt . expandMultiline opt . contextFilter (getLang opt filename) ((mkContextFilter opt) { getComment = False} ) $ text

    
    -- parse source code, get the Cpp.Token list...
    --
    
    let tokens = Cpp.tokenizer text'

    -- pre-process patterns
    --

    let patterns  = map (Cpp.tokenizer . contextFilter (Just Cpp) ((mkContextFilter opt) { getComment = False })) ps


    let patterns' = map (map mkWildCard) patterns >>= getWildCardSubsequence 

    -- get matching tokens ...
        
    let tokens' = sortBy (compare `on` Cpp.offset) $ nub (filterWildCardToken opt filename patterns' tokens)   

    
    let matches = map (\t -> let n = fromIntegral (Cpp.offset t) in (n, Cpp.toString t)) tokens' :: [(Int, String)]


    putStrLevel1 (debug opt) $ "strategy  : running C/C++ semantic search on " ++ filename ++ "..."
    putStrLevel2 (debug opt) $ "patterns  : " ++ show patterns'
    putStrLevel2 (debug opt) $ "tokens    : " ++ show tokens'
    putStrLevel2 (debug opt) $ "matches   : " ++ show matches 
    putStrLevel3 (debug opt) $ "---\n" ++ C.unpack text' ++ "\n---"
    
    return $ mkOutput opt filename text matches
        

instance GenericToken Cpp.Token where
    isIdentifier    = Cpp.isIdentifier
    getString       = Cpp.toString
    
    wildCardMatch _  (IdentifCard _) (Cpp.TokenIdentifier {}) = True
    wildCardMatch _  AnyCard _                                   = True
    wildCardMatch _  KeyWordCard (Cpp.TokenKeyword {}) = True
    wildCardMatch _  StringCard  (Cpp.TokenString  {}) = True
    wildCardMatch _  CharCard    (Cpp.TokenChar    {}) = True
    wildCardMatch _  NumberCard  (Cpp.TokenNumber  {}) = True
    wildCardMatch _  OctCard     (Cpp.TokenNumber  { Cpp.toString = r }) = case r of ('0':d: _) -> isDigit d; _ -> False
    wildCardMatch _  HexCard     (Cpp.TokenNumber  { Cpp.toString = r }) = case r of ('0':'x':_) -> True; _     -> False
    
    wildCardMatch opt (TokenCard Cpp.TokenIdentifier {Cpp.toString = l}) (Cpp.TokenIdentifier {Cpp.toString = r}) 
        | edit_dist  opt =  l ~== r
        | word_match opt =  l == r
        | otherwise      =  l `isInfixOf` r
    
    wildCardMatch opt (TokenCard Cpp.TokenString     {Cpp.toString = l}) (Cpp.TokenString     {Cpp.toString = r}) 
        | edit_dist  opt =  l ~== r
        | word_match opt =  l == r
        | otherwise      =  trim l `isInfixOf` r
            where trim = init . tail
    
    wildCardMatch _  (TokenCard l) r = Cpp.tokenCompare l r 
    wildCardMatch _ _ _ = False


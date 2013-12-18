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

module CGrep.Strategy.Tokenizer (searchTokenizer) where

import qualified Data.ByteString.Char8 as C
import qualified CGrep.Strategy.Cpp.Token as Cpp

import CGrep.Filter 
import CGrep.Lang
import CGrep.Common

import Options 
import Debug

import Data.List

searchTokenizer :: CgrepFunction
searchTokenizer opt ps f = do

    let filename = getFileName f 
    
    text <- getText (ignore_case opt) f 
    
    -- transform text
    
    let text' = getMultiLine (multiline opt) . filterContext (lookupLang filename) ((mkContextFilter opt) {getComment = False}) $ text
    
    -- parse source code, get the Cpp.Token list...
    
    let tokens = Cpp.tokenizer text'

    -- context-filterting...
       
    let tokens'= filter (Cpp.tokenFilter Cpp.TokenFilter { Cpp.filtIdentifier = identifier opt, 
                                                           Cpp.filtDirective  = directive opt,
                                                           Cpp.filtKeyword    = keyword opt,
                                                           Cpp.filtHeader     = header opt, 
                                                           Cpp.filtString     = string opt,
                                                           Cpp.filtNumber     = number opt,
                                                           Cpp.filtChar       = char opt,
                                                           Cpp.filtOper       = oper opt}) tokens

    -- filter tokens...
        
    let tokens'' = cppTokenFilter opt (map C.unpack ps) tokens'  

    -- convert Cpp.Tokens to CGrep.Tokens
    
    let matches = map (\t -> let off = fromIntegral (Cpp.offset t) in (off, Cpp.toString t)) tokens'' :: [(Int, String)]

    putStrLevel1 (debug opt) $ "strategy  : running C/C++ token search on " ++ filename ++ "..."
    putStrLevel2 (debug opt) $ "tokens    : " ++ show tokens
    putStrLevel2 (debug opt) $ "tokens'   : " ++ show tokens'
    putStrLevel2 (debug opt) $ "tokens''  : " ++ show tokens''
    putStrLevel2 (debug opt) $ "matches   : " ++ show matches

    putStrLevel3 (debug opt) $ "---\n" ++ C.unpack text' ++ "\n---"
    
    return $ mkOutput opt filename text matches
        

cppTokenFilter :: Options -> [String] -> [Cpp.Token] -> [Cpp.Token]
cppTokenFilter opt patterns tokens 
    | word_match opt = filter ((`elem` patterns) . Cpp.toString) tokens
    | otherwise      = filter ((\t -> any (`isInfixOf` t) patterns) . Cpp.toString) tokens



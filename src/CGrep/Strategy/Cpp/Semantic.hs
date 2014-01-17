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

import CGrep.WildCard
import CGrep.Filter 
import CGrep.Lang
import CGrep.Common
import CGrep.Output

import qualified Data.Map as M
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


    let patterns' = map (map mkWildCard) patterns >>= getOptionalSubsequence 

    -- get matching tokens ...
        
    let tokens' = sortBy (compare `on` Cpp.offset) $ nub (filterTokensWithWildCards opt patterns' tokens)   

    
    let matches = map (\t -> let n = fromIntegral (Cpp.offset t) in (n, Cpp.toString t)) tokens' :: [(Int, String)]


    putStrLevel1 (debug opt) $ "strategy  : running C/C++ semantic search on " ++ filename ++ "..."
    putStrLevel2 (debug opt) $ "patterns  : " ++ show patterns'
    putStrLevel2 (debug opt) $ "tokens    : " ++ show tokens'
    putStrLevel2 (debug opt) $ "matches   : " ++ show matches 
    putStrLevel3 (debug opt) $ "---\n" ++ C.unpack text' ++ "\n---"
    
    return $ mkOutput opt filename text matches
        

instance GenericToken Cpp.Token where
    tkIsIdentifier    = Cpp.isIdentifier
    tkIsString        = Cpp.isString
    tkIsChar          = Cpp.isChar
    tkIsNumber        = Cpp.isLiteralNumber
    tkIsKeyword       = Cpp.isKeyword
    
    tkToString       = Cpp.toString
    tkEquivalent     = Cpp.tokenCompare


wildCardMap :: M.Map String (WildCard a) 
wildCardMap = M.fromList 
            [
                ("ANY", AnyCard    ),
                ("KEY", KeyWordCard),
                ("OCT", OctCard    ),
                ("HEX", HexCard    ),
                ("NUM", NumberCard ),
                ("CHR", CharCard   ),
                ("STR", StringCard )
            ]


mkWildCard :: Cpp.Token -> WildCard Cpp.Token
mkWildCard t@(Cpp.TokenIdentifier s _) = 
    case () of 
        _  |  Just wc <-  M.lookup str wildCardMap -> wc
           | ('$':_)  <- s             -> IdentifCard str
           | ('_':_)  <- s             -> IdentifCard str
           | otherwise                 -> TokenCard t
    where str = tkToString t

mkWildCard t = TokenCard t



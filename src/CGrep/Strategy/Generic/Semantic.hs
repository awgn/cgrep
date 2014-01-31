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

module CGrep.Strategy.Generic.Semantic (search) where

import qualified Data.ByteString.Char8 as C

import CGrep.Filter
import CGrep.Lang
import CGrep.Common
import CGrep.Output

import CGrep.Semantic.Token
import CGrep.Semantic.WildCard

import qualified Data.Map as M

import Data.List
import Data.Function
import Data.Maybe

import Options
import Debug

import qualified CGrep.Semantic.Generic.Token as Generic

search :: CgrepFunction
search opt ps f = do

    let filename = getFileName f

    text <- getText f

    -- transform text

    let text' = expandMultiline opt . ignoreCase opt $ text

    -- pre-process patterns

    let patterns   = map (Generic.tokenizer . contextFilter (getLang opt filename) ((mkContextFilter opt) { getComment = False })) ps  -- [ [t1,t2,..], [t1,t2...] ]

    let patterns'  = map (map mkWildCard) patterns                 -- [ [w1,w2,..], [w1,w2,..] ]

    let patterns'' = map (combineMultiCard . map (:[])) patterns'  -- [ [m1,m2,..], [m1,m2,..] ] == [ [ [w1], [w2],..], [[w1],[w2],..]]

    -- quickSearch ...

    let ps' = map ( C.pack . maximumBy (compare `on` length) . mapMaybe (\x -> case x of
                                    TokenCard (Generic.TokenLiteral xs _) -> Just (unquotes $ trim xs)
                                    TokenCard t                           -> Just (tkToString t)
                                    _                                     -> Nothing)) patterns'

    -- put banners...

    putStrLevel1 (debug opt) $ "strategy  : running generic semantic search on " ++ filename ++ "..."
    putStrLevel2 (debug opt) $ "wildcards : " ++ show patterns'
    putStrLevel2 (debug opt) $ "multicards: " ++ show patterns''
    putStrLevel2 (debug opt) $ "identif   : " ++ show ps'

    let found = quickSearch opt ps' text'

    if maybe False not found
        then return $ mkOutput opt filename text []
        else do

            let text'' = contextFilter (getLang opt filename) ((mkContextFilter opt) {getComment = False }) text'

            -- parse source code, get the Generic.Token list...

            let tokens = Generic.tokenizer text''

            -- get matching tokens ...

            let tokens' = sortBy (compare `on` Generic.offset) $ nub $ concatMap (\ms -> filterTokensWithMultiCards opt ms tokens) patterns''

            let matches = map (\t -> let n = fromIntegral (Generic.offset t) in (n, Generic.toString t)) tokens' :: [(Int, String)]

            putStrLevel2 (debug opt) $ "tokens    : " ++ show tokens
            putStrLevel2 (debug opt) $ "tokens'   : " ++ show tokens'
            putStrLevel2 (debug opt) $ "matches   : " ++ show matches

            putStrLevel3 (debug opt) $ "---\n" ++ C.unpack text'' ++ "\n---"

            return $ mkOutput opt filename text matches


wildCardMap :: M.Map String (WildCard a)
wildCardMap = M.fromList
            [
                ("ANY", AnyCard     ),
                ("KEY", KeyWordCard ),
                ("OCT", OctCard     ),
                ("HEX", HexCard     ),
                ("NUM", NumberCard  ),
                ("CHR", CharCard    ),
                ("STR", StringCard  ),
                ("LIT", StringCard  )
            ]


mkWildCard :: Generic.Token -> WildCard Generic.Token
mkWildCard t@(Generic.TokenAlpha s _) =
    case () of
        _  |  Just wc <-  M.lookup str wildCardMap -> wc
           | ('$':_)  <- s             -> IdentifCard str
           | ('_':_)  <- s             -> IdentifCard str
           | otherwise                 -> TokenCard t
    where str = tkToString t
mkWildCard t = TokenCard t


combineMultiCard :: [MultiCard Generic.Token] -> [MultiCard Generic.Token]
combineMultiCard (m1:m2:m3:ms)
    | [TokenCard (Generic.TokenAlpha {Generic.toString = "OR"})] <- m2 =  combineMultiCard $ (m1++m3):ms
    | otherwise             =  m1 : combineMultiCard (m2:m3:ms)
combineMultiCard [m1,m2]    =  [m1,m2]
combineMultiCard [m1]       =  [m1]
combineMultiCard []         =  []



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

import qualified CGrep.Semantic.Cpp.Token  as Cpp

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

search :: CgrepFunction
search opt ps f = do

    let filename = getFileName f

    text <- getText f

    -- transform text

    let text' = ignoreCase opt text

        filt  = (mkContextFilter opt) { getComment = False }

    -- pre-process patterns

        patterns   = map (Cpp.tokenizer . contextFilter (Just Cpp) filt) ps  -- [ [t1,t2,..], [t1,t2...] ]
        patterns'  = map (map mkWildCard) patterns                           -- [ [w1,w2,..], [w1,w2,..] ]
        patterns'' = map (combineMultiCard . map (:[])) patterns'            -- [ [m1,m2,..], [m1,m2,..] ] == [ [ [w1], [w2],..], [[w1],[w2],..]]

    -- quick Search...

        ps' = map (C.pack . (\l ->  if null l then ""
                                              else maximumBy (compare `on` length) l) . mapMaybe (\x -> case x of
                                    TokenCard (Cpp.TokenChar    xs _) -> Just $ unquotes $ trim xs
                                    TokenCard (Cpp.TokenString  xs _) -> Just $ unquotes $ trim xs
                                    TokenCard t                       -> Just $ Cpp.toString t
                                    _                                 -> Nothing)) patterns'

        found = quickSearch opt ps' text'

    -- put banners...

    putStrLevel1 (debug opt) $ "strategy  : running C/C++ semantic search on " ++ filename ++ "..."
    putStrLevel2 (debug opt) $ "wildcards : " ++ show patterns'
    putStrLevel2 (debug opt) $ "multicards: " ++ show patterns''
    putStrLevel2 (debug opt) $ "identif   : " ++ show ps'

    if maybe False not found
        then return $ mkOutput opt filename text text []
        else do

            -- context filter

            let text'' = contextFilter (getLang opt filename) filt text'

            -- expand multi-line

                text''' = expandMultiline opt text''

            -- parse source code, get the Cpp.Token list...

                tokens = Cpp.tokenizer text'''

            -- get matching tokens ...

                tokens' = sortBy (compare `on` Cpp.toOffset) $ nub $ concatMap (\ms -> filterTokensWithMultiCards opt ms tokens) patterns''

                matches = map (\t -> let n = fromIntegral (Cpp.toOffset t) in (n, Cpp.toString t)) tokens' :: [(Int, String)]

            putStrLevel2 (debug opt) $ "tokens    : " ++ show tokens'
            putStrLevel2 (debug opt) $ "matches   : " ++ show matches
            putStrLevel3 (debug opt) $ "---\n" ++ C.unpack text''' ++ "\n---"

            return $ mkOutput opt filename text text''' matches


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
                ("LIT", LiteralCard )
            ]

mkWildCard :: Cpp.Token -> WildCard Cpp.Token
mkWildCard t@(Cpp.TokenIdentifier s off) =
    case () of
        _  |  Just wc <-  M.lookup str wildCardMap -> wc
           | isWildCardPattern s -> IdentifCard str
           | otherwise           -> TokenCard $ Cpp.TokenIdentifier (rmWildCardEscape s) off
    where str = tkToString t
mkWildCard t = TokenCard t


combineMultiCard :: [MultiCard Cpp.Token] -> [MultiCard Cpp.Token]
combineMultiCard (m1:r@(m2:m3:ms))
    | [TokenCard (Cpp.TokenIdentifier {Cpp.toString = "OR"})] <- m2 =  combineMultiCard $ (m1++m3):ms
    | otherwise          =  m1 : combineMultiCard r
combineMultiCard [m1,m2] =  [m1,m2]
combineMultiCard [m1]    =  [m1]
combineMultiCard []      =  []



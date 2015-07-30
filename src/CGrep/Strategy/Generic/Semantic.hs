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
import qualified CGrep.Semantic.Generic.Token as Generic

import CGrep.Filter
import CGrep.Lang
import CGrep.Common
import CGrep.Output

import CGrep.Semantic.Token
import CGrep.Semantic.WildCard

import Data.List
import Data.Function
import Data.Maybe

import Options
import Debug
import Util


search :: Options -> [Text8] -> FilePath -> IO [Output]
search opt ps f = do

    let filename = getTargetName f

    text <- getTargetContents f

    -- transform text

    let text' = ignoreCase opt text

        filt  = (mkContextFilter opt) { getComment = False }

    -- pre-process patterns

        patterns   = map (Generic.tokenizer . contextFilter (getFileLang opt filename) filt) ps  -- [ [t1,t2,..], [t1,t2...] ]
        patterns'  = map (map mkWildCardFromToken) patterns                                  -- [ [w1,w2,..], [w1,w2,..] ]
        patterns'' = map (combineMultiCard . map (:[])) patterns'                            -- [ [m1,m2,..], [m1,m2,..] ] == [[[w1], [w2],..], [[w1],[w2],..]]

    -- quickSearch ...

        ps' = filter (/= "OR") $ (mapMaybe (\x -> case x of
                                                    TokenCard (Generic.TokenLiteral xs _) -> Just (rmQuote $ trim xs)
                                                    TokenCard t                           -> Just (tkToString t)
                                                    _                                     -> Nothing
                                            ) . concat) patterns'

        found = quickSearch opt (map C.pack ps') text'

    -- put banners...

    putStrLevel1 (debug opt) $ "strategy  : running generic semantic search on " ++ filename ++ "..."
    putStrLevel2 (debug opt) $ "wildcards : " ++ show patterns'
    putStrLevel2 (debug opt) $ "multicards: " ++ show patterns''
    putStrLevel2 (debug opt) $ "identif   : " ++ show ps'


    if maybe False not found
        then return $ mkOutput opt filename text text []
        else do

            -- context filter

            let text'' = contextFilter (getFileLang opt filename) filt text'

            -- expand multi-line

                text''' = expandMultiline opt text''

            -- parse source code, get the Generic.Token list...

                tokens = Generic.tokenizer text'''

            -- get matching tokens ...

                tokens' = sortBy (compare `on` Generic.toOffset) $ nub $ concatMap (\ms -> filterTokensWithMultiCards opt ms tokens) patterns''

                matches = map (\t -> let n = fromIntegral (Generic.toOffset t) in (n, Generic.toString t)) tokens' :: [(Int, String)]

            putStrLevel2 (debug opt) $ "tokens    : " ++ show tokens'
            putStrLevel2 (debug opt) $ "matches   : " ++ show matches
            putStrLevel3 (debug opt) $ "---\n" ++ C.unpack text''' ++ "\n---"

            return $ mkOutput opt filename text text''' matches


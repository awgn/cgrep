--
-- Copyright (c) 2013-2019 Nicola Bonelli <nicola@pfq.io>
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

import qualified CGrep.Parser.Generic.Token as Generic

import CGrep.Filter
    ( ContextFilter(getFilterComment), mkContextFilter, contextFilter )
import CGrep.Lang ( getFileLang )
import CGrep.Common
    ( Text8,
      trim,
      getTargetName,
      getTargetContents,
      shallowSearch,
      runSearch,
      expandMultiline,
      ignoreCase )
import CGrep.Output ( Output, mkOutput )

import CGrep.Parser.Token ( SemanticToken(tkToString) )
import CGrep.Parser.WildCard
    ( WildCard(TokenCard),
      mkWildCardFromToken,
      combineMultiCard,
      filterTokensWithMultiCards )

import Control.Monad.Trans.Reader ( reader )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Data.List ( sortBy, nub )
import Data.Function ( on )
import Data.Maybe ( mapMaybe )

import Reader ( OptionIO )
import Verbose
import Util ( notNull, rmQuote )


search :: FilePath -> [Text8] -> OptionIO [Output]
search f ps = do

    opt  <- reader snd
    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let [text''', text'', text', _ ] = scanr ($) text [ expandMultiline opt
                                                      , contextFilter (getFileLang opt filename) filt
                                                      , ignoreCase opt
                                                      ]

        filt  = (mkContextFilter opt) { getFilterComment = False }

    -- pre-process patterns

        patterns   = map (Generic.tokenizer . contextFilter (getFileLang opt filename) filt) ps  -- [ [t1,t2,..], [t1,t2...] ]
        patterns'  = map (map mkWildCardFromToken) patterns                                      -- [ [w1,w2,..], [w1,w2,..] ]
        patterns'' = map (combineMultiCard . map (:[])) patterns'                                -- [ [m1,m2,..], [m1,m2,..] ] == [[[w1], [w2],..], [[w1],[w2],..]]

        identif = mapMaybe (\case
                            TokenCard (Generic.TokenLiteral xs _) -> Just (rmQuote $ trim xs)
                            TokenCard (Generic.TokenAlpha "OR" _) -> Nothing
                            TokenCard t                           -> Just (tkToString t)
                            _                                     -> Nothing
                            ) . concat $ patterns'

    -- put banners...

    putStrLn1 $ "strategy  : running generic semantic search on " ++ filename ++ "..."
    putStrLn2 $ "wildcards : " ++ show patterns'
    putStrLn2 $ "multicards: " ++ show patterns''
    putStrLn2 $ "identif   : " ++ show identif

    let idpack = map C.pack identif
        quick1 = all notNull $ shallowSearch idpack text'
        quick2 = all notNull $ shallowSearch idpack text''

    runSearch opt filename (quick1 && quick2) $ do

        -- parse source code, get the Generic.Token list...

        let tokens = Generic.tokenizer text'''

        -- get matching tokens ...

        let tokens' = sortBy (compare `on` Generic.toOffset) $ nub $ concatMap (\ms -> filterTokensWithMultiCards opt ms tokens) patterns''
        let matches = map (\t -> let n = fromIntegral (Generic.toOffset t) in (n, Generic.toString t)) tokens' :: [(Int, String)]

        putStrLn2 $ "tokens    : " ++ show tokens'
        putStrLn2 $ "matches   : " ++ show matches
        putStrLn3 $ "---\n" ++ C.unpack text''' ++ "\n---"

        mkOutput filename text text''' matches

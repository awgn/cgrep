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

module CGrep.Strategy.Cpp.Semantic (search) where

import qualified Data.ByteString.Char8 as C

import qualified CGrep.Parser.Cpp.Token  as Cpp

import Control.Monad.Trans.Reader ( reader )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.List ( sortBy, nub )
import Data.Function ( on )
import Data.Maybe ( mapMaybe )

import CGrep.ContextFilter
    ( ContextFilter(getFilterComment), mkContextFilter)
import CGrep.Languages ( Language(Cpp) )
import CGrep.LanguagesMap ( languageLookup, contextFilter)

import CGrep.Common
    ( Text8,
      expandMultiline,
      getTargetContents,
      getTargetName,
      ignoreCase,
      runSearch,
      shallowSearch,
      quickMatch,
      trim )
import CGrep.Output ( Output, mkOutput )

import CGrep.Parser.WildCard
    ( combineMultiCard,
      filterTokensWithMultiCards,
      mkWildCardFromToken,
      WildCard(TokenCard) )

import Reader ( OptionIO )
import Verbose ( putStrLn1, putStrLn2, putStrLn3 )
import Util ( notNull, rmQuote )
import CGrep.Token (Token (Token))


search :: FilePath -> [Text8] -> OptionIO [Output]
search f patterns = do

    opt  <- reader snd
    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let filt = (mkContextFilter opt) { getFilterComment = False }


    let [text''', text'' , text', _] = scanr ($) text [ expandMultiline opt
                                                      , contextFilter (languageLookup opt filename) filt
                                                      , ignoreCase opt
                                                      ]

    -- pre-process patterns

        patterns'   = map (Cpp.tokenizer . contextFilter (Just Cpp) filt) patterns    -- [ [t1,t2,..], [t1,t2...] ]
        patterns''  = map (map mkWildCardFromToken) patterns'                         -- [ [w1,w2,..], [w1,w2,..] ]
        patterns''' = map (combineMultiCard . map (:[])) patterns''                   -- [ [m1,m2,..], [m1,m2,..] ] == [ [ [w1], [w2],..], [[w1],[w2],..]]

        identif = mapMaybe (\case
                              TokenCard (Cpp.TokenChar   xs _) -> Just (rmQuote $ trim xs)
                              TokenCard (Cpp.TokenString xs _) -> Just (rmQuote $ trim xs)
                              TokenCard (Cpp.TokenIdentifier "OR" _) -> Nothing
                              TokenCard t                            -> Just (Cpp.toString t)
                              _                                      -> Nothing
                  ) . concat $ patterns''

    -- put banners...

    putStrLn1 $ "strategy  : running C/C++ semantic search on " ++ filename ++ "..."
    putStrLn2 $ "wildcards : " ++ show patterns''
    putStrLn2 $ "multicards: " ++ show patterns'''
    putStrLn2 $ "identif   : " ++ show identif

    let idpack = map C.pack identif
        quick1 = quickMatch patterns $ shallowSearch idpack text'
        quick2 = quickMatch patterns $ shallowSearch idpack text''

    runSearch opt filename (quick1 && quick2) $ do

        -- parse source code, get the Cpp.Token list...

        let tokens = Cpp.tokenizer text'''

        -- get matching tokens ...

        let tokens' = sortBy (compare `on` Cpp.toOffset) $ nub $ concatMap (\ms -> filterTokensWithMultiCards opt ms tokens) patterns'''

        let matches = map (\t -> let n = fromIntegral (Cpp.toOffset t) in Token n (C.pack (Cpp.toString t))) tokens' :: [Token]

        putStrLn2 $ "tokens    : " ++ show tokens'
        putStrLn2 $ "matches   : " ++ show matches
        putStrLn3 $ "---\n" ++ C.unpack text''' ++ "\n---"

        mkOutput filename text text''' matches

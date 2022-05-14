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

{-# LANGUAGE RecordWildCards #-}

module CGrep.Strategy.BoyerMoore (search) where

import qualified Data.ByteString.Char8 as C

import Control.Monad.Trans.Reader ( reader, ask )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.List ( isSuffixOf, isPrefixOf, genericLength )

import CGrep.Common
    ( Text8,
      expandMultiline,
      getTargetContents,
      getTargetName,
      ignoreCase,
      runSearch,
      shallowSearch, quickMatch )
import CGrep.Output ( Output, mkOutput )
import CGrep.ContextFilter ( mkContextFilter )
import CGrep.LanguagesMap ( languageLookup, contextFilter, LanguageInfo )
import CGrep.Types ( Offset )

import CGrep.Parser.Chunk ( parseChunks )

import Reader ( OptionIO, Env(..) )
import Options ( Options(word_match, prefix_match, suffix_match) )
import Verbose ( putStrLn1, putStrLn2, putStrLn3 )
import Util ( notNull )
import CGrep.Chunk (Chunk(..))


search :: FilePath -> [Text8] -> OptionIO [Output]
search f patterns = do

    Env{..} <- ask
    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let [text''', _ , _ , _] = scanr ($) text [ expandMultiline opt
                                              , contextFilter (languageLookup opt filename) (mkContextFilter opt)
                                              , ignoreCase opt
                                              ]

    -- make shallow search

    let shallow = shallowSearch patterns text'''

    -- search for matching tokens

    let tokens = concatMap (\(p, xs) -> (`Chunk` p) <$> xs ) $ zip patterns shallow

    -- filter exact/partial matching tokens

    let tokens' = if word_match opt || prefix_match opt || suffix_match opt
                    then filter (checkChunk opt langInfo text''') tokens
                    else tokens

    putStrLn1 $ "strategy  : running Boyer-Moore search on " <> filename <> "..."

    runSearch opt filename (quickMatch patterns shallow) $ do

        -- print banners...

        putStrLn2 $ "tokens    : " <> show tokens
        putStrLn2 $ "tokens'   : " <> show tokens'
        putStrLn3 $ "---\n" <> C.unpack text''' <> "\n---"

        mkOutput filename text text''' tokens'


checkChunk :: Options -> Maybe LanguageInfo -> Text8 -> Chunk -> Bool
checkChunk opt linfo text Chunk{..}
     | word_match    opt = Chunk (tOffset - off') tStr `elem` cs
     | prefix_match  opt = any (\(Chunk o s) -> tStr `C.isPrefixOf` s && o + off' == tOffset) cs
     | suffix_match  opt = any (\(Chunk o s) -> tStr `C.isSuffixOf` s && o + off' + fromIntegral (C.length s - C.length tStr) == tOffset) cs
     | otherwise         = undefined
     where (text',off') = getLineByOffset tOffset text
           cs           = parseChunks linfo text'


splitLines :: Text8 -> [(Text8, Offset)]
splitLines xs = zip ls off
    where ls  = C.lines xs
          off = fromIntegral<$> scanl (\o l -> 1 + o + C.length l) 0 ls
{-# INLINE splitLines #-}


getLineByOffset :: Offset -> Text8 -> (Text8, Offset)
getLineByOffset off xs = last $ takeWhile (\(_,o) -> o <= off) sl
        where sl = splitLines xs
{-# INLINE getLineByOffset #-}

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
{-# LANGUAGE ScopedTypeVariables #-}

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
      stringSearch, quickMatch )
import CGrep.Output ( Output, mkOutputElements )
import CGrep.ContextFilter ( mkContextFilter )
import CGrep.Language ( Language )
import CGrep.LanguagesMap ( languageLookup, contextFilter, LanguageInfo )
import CGrep.Types ( Offset )

import CGrep.Parser.Chunk ( parseChunks )

import Reader ( ReaderIO, Env(..) )
import Options ( Options(word_match, prefix_match, suffix_match) )
import Verbose ( putStrLnVerbose )
import Util ( notNull )
import CGrep.Chunk (Chunk(..))
import Data.Int

import System.Posix.FilePath ( RawFilePath )

search :: Maybe (Language, LanguageInfo) -> RawFilePath -> [Text8] -> ReaderIO [Output]
search linfo f patterns = do

    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let ctxFilter = mkContextFilter opt

    let [text', _ , _ , _] = scanr ($) text [ expandMultiline opt
                                            , contextFilter (fst <$> linfo) ctxFilter False
                                            , ignoreCase opt
                                            ]

    -- make shallow search

    let shallow = stringSearch patterns text'
    -- let shallow :: [[Int64]] = []

    -- search for matching tokens

    let tokens = concatMap (\(p, xs) -> (`Chunk` p) <$> xs ) $ zip patterns shallow

    -- filter exact/partial matching tokens

    let tokens' = if word_match opt || prefix_match opt || suffix_match opt
                    then filter (checkChunk opt (snd <$> linfo) text') tokens
                    else tokens

    putStrLnVerbose 2 $ "strategy  : running Boyer-Moore search on " <> C.unpack filename
    putStrLnVerbose 3 $ "---\n" <> C.unpack text' <> "\n---"

    runSearch opt filename (quickMatch patterns shallow) $ do
        putStrLnVerbose 2 $ "tokens'   : " <> show tokens'
        mkOutputElements filename text text' tokens'


checkChunk :: Options -> Maybe LanguageInfo -> Text8 -> Chunk -> Bool
checkChunk opt linfo text Chunk{..}
     | word_match    opt = Chunk (tOffset - off') tStr `elem` cs
     | prefix_match  opt = any (\(Chunk o s) -> tStr `C.isPrefixOf` s && o + off' == tOffset) cs
     | suffix_match  opt = any (\(Chunk o s) -> tStr `C.isSuffixOf` s && o + off' + fromIntegral (C.length s - C.length tStr) == tOffset) cs
     | otherwise         = undefined
     where (text',off')  = getLineByOffset tOffset text
           cs            = parseChunks linfo text'


splitLines :: Text8 -> [(Text8, Offset)]
splitLines xs = zip ls off
    where ls  = C.lines xs
          off = fromIntegral<$> scanl (\o l -> 1 + o + C.length l) 0 ls
{-# INLINE splitLines #-}


getLineByOffset :: Offset -> Text8 -> (Text8, Offset)
getLineByOffset off xs = last $ takeWhile (\(_,o) -> o <= off) sl
        where sl = splitLines xs
{-# INLINE getLineByOffset #-}

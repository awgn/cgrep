--
-- Copyright (c) 2013-2023 Nicola Bonelli <nicola@larthia.com>
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

module CGrep.Strategy.Tokenizer (search) where

import CGrep.Common (
    expandMultiline,
    getTargetContents,
    getTargetName,
    ignoreCase,
    subText,
 )
import CGrep.ContextFilter (
    contextBitComment,
    mkContextFilter,
    (~!),
 )
import CGrep.Distance ((~==))
import CGrep.FileType (FileType)
import CGrep.FileTypeMap (
    FileTypeInfo,
 )

import CGrep.FileTypeMapTH (
    mkContextFilterFn,
 )

import CGrep.Common (runSearch)
import CGrep.Line
import CGrep.Match (Match, mkMatches)
import CGrep.Parser.Chunk (Chunk (..))
import CGrep.Parser.Token
import CGrep.Text (textContainsOneOf, textIndices)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ask)
import Data.Coerce (coerce)
import qualified Data.Text as T
import Options (
    Options (
        edit_dist,
        identifier,
        keyword,
        nativeType,
        number,
        operator,
        prefix_match,
        string,
        suffix_match,
        word_match
    ),
 )
import PutMessage (putMessageLnVerb)
import Reader (Env (..), ReaderIO)
import System.IO (stderr)
import System.OsPath (OsPath)
import Util (mapMaybe')
import Control.Concurrent (MVar)

search :: MVar () -> Maybe (FileType, FileTypeInfo) -> OsPath -> [T.Text] -> Bool -> ReaderIO [Match]
search lock info f patterns strict = do
    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f
    let lindex = buildIndex text

    -- transform text

    let filt = mkContextFilter opt ~! contextBitComment

    let !contextFilter = mkContextFilterFn (fst <$> info) filt False

    let text' = ignoreCase opt text
    let text'' = expandMultiline opt . contextFilter $ text'

    -- make shallow search
    let !eligibleForSearch = textContainsOneOf patterns text'

    putMessageLnVerb 1 lock stderr $ "strategy: running token search on " <> show filename <> "..."
    putMessageLnVerb 3 lock stderr $ "---\n" <> text'' <> "\n---"

    runSearch opt lindex filename eligibleForSearch $ do
        let indices = textIndices patterns text''

        -- parse source code, get the token list...

        let tfilter =
                TokenFilter
                    { tfIdentifier = identifier opt
                    , tfKeyword = keyword opt
                    , tfNativeType = nativeType opt
                    , tfString = string opt
                    , tfNumber = number opt
                    , tfOperator = operator opt
                    , tfBracket = False
                    }

        let tokens = parseTokens tfilter (snd <$> info) strict (subText indices text'')

            -- filter tokens and make chunks

            chunks = mapMaybe' (tokenizerFilter opt patterns) tokens

        putMessageLnVerb 2 lock stderr $ "tokens    : " <> show tokens
        putMessageLnVerb 2 lock stderr $ "matches   : " <> show chunks

        mkMatches lindex filename text'' chunks

tokenizerFilter :: Options -> [T.Text] -> Token -> Maybe Chunk
tokenizerFilter opt patterns token
    | isTokenUnspecified token = Nothing
    | tokenPredicate opt patterns token = Just $ coerce token
    | otherwise = Nothing
{-# INLINE tokenizerFilter #-}

tokenPredicate :: Options -> [T.Text] -> Token -> Bool
tokenPredicate opt patterns tokens
    | edit_dist opt = (\t -> any (\p -> T.unpack p ~== (T.unpack . tToken) t) patterns) tokens
    | word_match opt = ((`elem` patterns) . tToken) tokens
    | prefix_match opt = ((\t -> any (`T.isPrefixOf` t) patterns) . tToken) tokens
    | suffix_match opt = ((\t -> any (`T.isSuffixOf` t) patterns) . tToken) tokens
    | otherwise = ((\t -> any (`T.isInfixOf` t) patterns) . tToken) tokens

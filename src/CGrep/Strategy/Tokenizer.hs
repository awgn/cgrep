--
-- Copyright (c) 2013-2025 Nicola Bonelli <nicola@larthia.com>
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
    ignoreCase,
    sliceToMaxIndex,
 )
import CGrep.Semantic.ContextFilter (
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
import Control.Concurrent (MVar)
import Control.Monad.Trans.Reader (ask)
import Data.Coerce (coerce)
import qualified Data.Text as T
import Options (
    Options (
        edit_dist,
        identifier,
        ignore_case,
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

search :: MVar () -> Maybe (FileType, FileTypeInfo) -> OsPath -> T.Text -> [T.Text] -> Bool -> ReaderIO [Match]
search lock info filename text patterns strict = do
    Env{..} <- ask
    let lindex = buildIndex text

    -- transform text

    let filt = mkContextFilter opt ~! contextBitComment

    let !contextFilter = mkContextFilterFn (fst <$> info) filt True

    let text'' = contextFilter text

    -- make shallow search
    let !eligibleForSearch = edit_dist opt || textContainsOneOf patterns (ignoreCase opt text)

    putMessageLnVerb 3 lock stderr $ "---\n" <> text'' <> "\n---"
    putMessageLnVerb 1 lock stderr $ "strategy: running token search on " <> show filename <> "..."

    runSearch lindex filename eligibleForSearch $ do
        let textToParse
                | ignore_case opt || edit_dist opt = text''
                | otherwise =
                    let indices = textIndices patterns text''
                     in sliceToMaxIndex indices text''

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

        let tokens = parseTokens tfilter (snd <$> info) strict textToParse

            -- filter tokens and make chunks

            chunks = coerce (filter (tokenPredicate opt patterns) tokens) :: [Chunk]

        putMessageLnVerb 2 lock stderr $ "tokens    : " <> show tokens
        putMessageLnVerb 2 lock stderr $ "matches   : " <> show chunks

        mkMatches lindex filename text'' chunks

tokenPredicate :: Options -> [T.Text] -> Token -> Bool
tokenPredicate opt patterns token
    | edit_dist opt = any (\p -> T.unpack p ~== T.unpack tokTxt) patterns
    | word_match opt = tokTxt `elem` patterns
    | prefix_match opt = any (`T.isPrefixOf` tokTxt) patterns
    | suffix_match opt = any (`T.isSuffixOf` tokTxt) patterns
    | otherwise = any (`T.isInfixOf` tokTxt) patterns
  where
    tokTxt = ignoreCase opt (tToken token)

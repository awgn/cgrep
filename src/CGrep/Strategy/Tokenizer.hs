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
    runSearch,
    sliceToLastMatch,
    textContainsOneOfOpt,
 )
import CGrep.Semantic.ContextFilter (
    contextBitComment,
    mkContextFilter,
    (~!),
 )
import CGrep.Distance (matchEditPattern, mkEditPattern)
import CGrep.FileType (FileType)
import CGrep.FileTypeMap (
    FileTypeInfo,
 )

import CGrep.FileTypeMapTH (
    mkContextFilterFn,
 )

import CGrep.Line
import CGrep.Match (Match, mkMatches)
import CGrep.Parser.Chunk (Chunk (..))
import CGrep.Parser.Token
import CGrep.Text (textIsInfixOf)
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
    let !eligibleForSearch = edit_dist opt || textContainsOneOfOpt opt patterns text

    putMessageLnVerb 3 lock stderr $ "---\n" <> text'' <> "\n---"
    putMessageLnVerb 1 lock stderr $ "strategy: running token search on " <> show filename <> "..."

    runSearch lindex filename eligibleForSearch $ do
        let textToParse
                | ignore_case opt || edit_dist opt = text''
                | otherwise = sliceToLastMatch patterns text''

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
            !predFn = mkTokenPredicate opt patterns

            -- filter tokens and make chunks

            chunks = coerce (filter predFn tokens) :: [Chunk]

        putMessageLnVerb 2 lock stderr $ "tokens    : " <> show tokens
        putMessageLnVerb 2 lock stderr $ "matches   : " <> show chunks

        mkMatches lindex filename text'' chunks

mkTokenPredicate :: Options -> [T.Text] -> (Token -> Bool)
mkTokenPredicate opt patterns
    | edit_dist opt =
        let !editPatterns = map mkEditPattern patterns
         in case editPatterns of
                [ep] -> \tok -> matchEditPattern ep (ignoreCase opt (tToken tok))
                eps -> \tok -> let !tokTxt = ignoreCase opt (tToken tok) in any (`matchEditPattern` tokTxt) eps
    | word_match opt =
        case patterns of
            [p] -> \tok -> ignoreCase opt (tToken tok) == p
            ps -> \tok -> ignoreCase opt (tToken tok) `elem` ps
    | prefix_match opt =
        case patterns of
            [p] -> \tok -> p `T.isPrefixOf` ignoreCase opt (tToken tok)
            ps -> \tok -> let !tokTxt = ignoreCase opt (tToken tok) in any (`T.isPrefixOf` tokTxt) ps
    | suffix_match opt =
        case patterns of
            [p] -> \tok -> p `T.isSuffixOf` ignoreCase opt (tToken tok)
            ps -> \tok -> let !tokTxt = ignoreCase opt (tToken tok) in any (`T.isSuffixOf` tokTxt) ps
    | otherwise =
        case patterns of
            [p] -> \tok -> p `textIsInfixOf` ignoreCase opt (tToken tok)
            ps -> \tok -> let !tokTxt = ignoreCase opt (tToken tok) in any (`textIsInfixOf` tokTxt) ps

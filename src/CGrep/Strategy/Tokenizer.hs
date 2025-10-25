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
    fileTypeLookup,
 )

import CGrep.Match (Match, mkMatches)
import CGrep.Common(runSearch)
import CGrep.Parser.Chunk (Chunk (..))
import CGrep.Parser.Token
import CGrep.Line
import CGrep.Text (textIndices)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ask, reader)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (toList))
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.Sequence as S
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
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as UV

search :: Maybe (FileType, FileTypeInfo) -> OsPath -> [T.Text] -> Bool -> ReaderIO [Match]
search info f patterns strict = do
    undefined

-- search :: Maybe (FileType, FileTypeInfo) -> OsPath -> [T.Text] -> Bool -> ReaderIO [OutputMatch]
-- search info f ps strict = do
--     Env{..} <- ask
--
--     text <- liftIO $ getTargetContents f
--
--     let filename = getTargetName f
--
--     -- transform text
--
--     let filt = mkContextFilter opt ~! contextBitComment
--
--     let [text''', _, text', _] =
--             scanr
--                 ($)
--                 text
--                 [ expandMultiline opt
--                 , contextFilter (fst <$> fileTypeLookup opt filename) filt True
--                 , ignoreCase opt
--                 ]
--
--     putMessageLnVerb 1 stderr $ "strategy: running token search on " <> show filename <> "..."
--     putMessageLnVerb 3 stderr $ "---\n" <> text''' <> "\n---"
--
--     let indices' = textIndices ps text'
--
--     runSearch opt filename (eligibleForSearch ps indices') $ do
--         -- parse source code, get the token list...
--
--         let tfilter =
--                 TokenFilter
--                     { tfIdentifier = identifier opt
--                     , tfKeyword = keyword opt
--                     , tfNativeType = nativeType opt
--                     , tfString = string opt
--                     , tfNumber = number opt
--                     , tfOperator = operator opt
--                     , tfBracket = False
--                     }
--
--         let tokens = {-# SCC "tok_0" #-} parseTokens tfilter (snd <$> info) strict (subText indices' text''')
--
--             -- filter tokens and make chunks
--
--             matches = {-# SCC "tok_3" #-} mapMaybe' (tokenizerFilter opt ps) tokens
--
--         putMessageLnVerb 2 stderr $ "tokens    : " <> show tokens
--         putMessageLnVerb 2 stderr $ "matches   : " <> show matches
--
--         let lineOffsets = getLineOffsets text
--
--         mkOutputMatches lineOffsets filename text text''' matches

-- tokenizerFilter :: Options -> [T.Text] -> Token -> Maybe Chunk
-- tokenizerFilter opt patterns token
--     | isTokenUnspecified token = Nothing
--     | tokenPredicate opt patterns token = Just $ coerce token
--     | otherwise = Nothing
-- {-# INLINE tokenizerFilter #-}
--
-- tokenPredicate :: Options -> [T.Text] -> Token -> Bool
-- tokenPredicate opt patterns tokens
--     | edit_dist opt = (\t -> any (\p -> T.unpack p ~== (T.unpack . tToken) t) patterns) tokens
--     | word_match opt = ((`elem` patterns) . tToken) tokens
--     | prefix_match opt = ((\t -> any (`T.isPrefixOf` t) patterns) . tToken) tokens
--     | suffix_match opt = ((\t -> any (`T.isSuffixOf` t) patterns) . tToken) tokens
--     | otherwise = ((\t -> any (`T.isInfixOf` t) patterns) . tToken) tokens

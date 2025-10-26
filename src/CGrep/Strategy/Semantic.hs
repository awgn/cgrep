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

module CGrep.Strategy.Semantic (search) where

import CGrep.Common (expandMultiline, getTargetContents, getTargetName, ignoreCase, runSearch, subText, trimT)
import CGrep.ContextFilter (
    contextBitComment,
    mkContextFilter,
    (~!),
 )
import CGrep.FileType (FileType)
import CGrep.FileTypeMap (
    FileTypeInfo,
 )
import CGrep.FileTypeMapTH (
    mkContextFilterFn,
 )
import CGrep.Line (buildIndex)
import CGrep.Match (Match, mkMatches)
import CGrep.Parser.Atom (
    Atom (..),
    findAllMatches,
    mkAtomFromToken,
 )
import CGrep.Parser.Chunk
import CGrep.Parser.Token
import CGrep.Text (textContainsOneOf, textIndices)
import Control.Concurrent (MVar)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ask)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (toList))
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import PutMessage (putMessageLnVerb)
import Reader (Env (..), ReaderIO)
import System.IO (stderr)
import System.OsPath (OsPath)
import Util (unquoteT)

search :: MVar () -> Maybe (FileType, FileTypeInfo) -> OsPath -> [T.Text] -> Bool -> ReaderIO [Match]
search lock info f patterns strict = do
    Env{..} <- ask

    text <- liftIO $ getTargetContents f
    let filename = getTargetName f
    let lindex = buildIndex text

    let filt = mkContextFilter opt ~! contextBitComment
    let !contextFilter = mkContextFilterFn (fst <$> info) filt True

    let text' = ignoreCase opt text
    let text'' = expandMultiline opt . contextFilter $ text'

    -- pre-process patterns
    let pfilter =
            TokenFilter
                { tfIdentifier = True
                , tfKeyword = True
                , tfNativeType = True
                , tfString = True
                , tfNumber = True
                , tfOperator = True
                , tfBracket = True
                }

    let patterns' = map (parseTokens pfilter (snd <$> info) strict . contextFilter) patterns
        patterns'' = map (toList . (mkAtomFromToken <$>)) patterns'

    let matchers =
            mapMaybe
                ( \case
                    Exact (Token (Chunk ChunkString xs)) -> Just ((unquoteT . trimT) xs)
                    Exact t -> Just (tToken t)
                    _ -> Nothing
                )
                (concatMap toList patterns'')

    let !eligibleForSearch = textContainsOneOf matchers text'

    runSearch opt lindex filename eligibleForSearch $ do
        putMessageLnVerb 3 lock stderr $ "---\n" <> text'' <> "\n---"
        putMessageLnVerb 1 lock stderr $ "strategy  : running generic semantic search on " <> show filename
        putMessageLnVerb 2 lock stderr $ "atoms     : " <> show patterns''
        putMessageLnVerb 2 lock stderr $ "matchers  : " <> show matchers

        -- parse source code, get the Generic.Chunk list...
        let indices' = textIndices matchers text''

        -- parse source code, get the Generic.Token list...
        let tfilter = mkTokenFilter $ cTyp . coerce <$> concatMap toList patterns'
        putMessageLnVerb 3 lock stderr $ "filter    : " <> show tfilter

        let tokens = toList $ parseTokens tfilter (snd <$> info) strict (subText indices' text'')
        putMessageLnVerb 3 lock stderr $ "tokens    : " <> show tokens

        -- get matching tokens ...

        let allMatches = sortBy (compare `on` tOffset) $ findAllMatches opt patterns'' tokens

        -- convert Tokens to Chunks
        let matches = coerce allMatches :: [Chunk]
        putMessageLnVerb 2 lock stderr $ "matches   : " <> show matches

        mkMatches lindex filename text'' matches

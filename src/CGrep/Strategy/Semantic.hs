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

module CGrep.Strategy.Semantic (search) where

import CGrep.Common (
    expandMultiline,
    getTargetContents,
    getTargetName,
    ignoreCase,
    subText,
    trim,
    trimT,
 )
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
    contextFilter,
    fileTypeLookup,
 )
import CGrep.Match (Match, mkMatches)
import CGrep.Parser.Atom (
    Atom (..),
    findAllMatches,
    mkAtomFromToken,
 )
import CGrep.Common(runSearch)
import CGrep.Parser.Chunk
import CGrep.Parser.Token
import CGrep.Common (eligibleForSearch)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ask, reader)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (toList))
import Data.Function (on)
import Data.List (nub, sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as S
import qualified Data.Vector.Unboxed as UV
import PutMessage (putMessageLnVerb)
import Reader (Env (..), ReaderIO)
import System.IO (stderr)
import System.OsPath (OsPath, takeBaseName)
import Util (unquoteT)
import qualified Data.Text as T
import CGrep.Line (getLineOffsets)
import CGrep.Text (textIndices)

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
--     let [text''', _, text', _] =
--             scanr
--                 ($)
--                 text
--                 [ expandMultiline opt
--                 , contextFilter (fst <$> fileTypeLookup opt filename) filt True
--                 , ignoreCase opt
--                 ]
--
--         filt = mkContextFilter opt ~! contextBitComment
--
--         -- pre-process patterns
--
--         pfilter =
--             TokenFilter
--                 { tfIdentifier = True
--                 , tfKeyword = True
--                 , tfNativeType = True
--                 , tfString = True
--                 , tfNumber = True
--                 , tfOperator = True
--                 , tfBracket = True
--                 }
--
--         patterns = map (parseTokens pfilter (snd <$> info) strict . contextFilter (fst <$> fileTypeLookup opt filename) filt True) ps
--         patterns' = map (mkAtomFromToken <$>) patterns
--         patterns'' = map toList patterns'
--
--         matchers =
--             mapMaybe
--                 ( \case
--                     Exact (Token (Chunk ChunkString xs _)) -> Just ((unquoteT . trimT) xs)
--                     Exact t -> Just (tToken t)
--                     _ -> Nothing
--                 )
--                 (concatMap toList patterns')
--
--     -- put banners...
--
--     putMessageLnVerb 3 stderr $ "---\n" <> text''' <> "\n---"
--     putMessageLnVerb 1 stderr $ "strategy  : running generic semantic search on " <> show filename
--     putMessageLnVerb 2 stderr $ "atoms     : " <> show patterns''
--     putMessageLnVerb 2 stderr $ "matchers  : " <> show matchers
--
--     -- parse source code, get the Generic.Chunk list...
--     let indices' = textIndices matchers text'
--
--     let eligible_for_search = eligibleForSearch matchers indices'
--     runSearch opt filename eligible_for_search $ do
--         -- parse source code, get the Generic.Token list...
--         let tfilter = mkTokenFilter $ cTyp . coerce <$> concatMap toList patterns
--         putMessageLnVerb 3 stderr $ "filter    : " <> show tfilter
--
--         let tokens = toList $ parseTokens tfilter (snd <$> info) strict (subText indices' text''')
--         putMessageLnVerb 3 stderr $ "tokens    : " <> show tokens
--
--         -- get matching tokens ...
--
--         let allMatches = sortBy (compare `on` tOffset) $ nub $ findAllMatches opt patterns'' tokens
--
--         -- convert Tokens to Chunks
--
--         let matches = coerce allMatches :: [Chunk]
--         putMessageLnVerb 2 stderr $ "matches   : " <> show matches
--
--         let lineOffsets = getLineOffsets text
--         mkOutputMatches lineOffsets filename text text''' matches

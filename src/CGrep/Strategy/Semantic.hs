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

import CGrep.Common (runSearch, sliceToLastMatch, textContainsOneOfOpt, trimT)
import CGrep.Semantic.ContextFilter (
    contextBitComment,
    contextFilterAll,
    mkContextFilter,
    (~!),
 )
import CGrep.FileType (FileType)
import CGrep.FileTypeMap (
    FileTypeInfo (..),
    FileTypeInfoMap (..),
    WordType (..),
 )
import CGrep.FileTypeMapTH (
    fileTypeInfoMap,
    mkContextFilterFn,
 )
import CGrep.Line (buildIndex)
import CGrep.Match (Match, mkMatches)
import CGrep.Parser.Atom (
    Atom (Exact, Not, Or),
    extractAtomMatchers,
    findAllMatches,
    mkAtomsFromTokens,
 )
import CGrep.Parser.Chunk
import CGrep.Parser.Token
import CGrep.Text (textIndices)
import Control.Concurrent (MVar)
import Control.Monad.Trans.Reader (ask)
import Data.Coerce (coerce)
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import PutMessage (putMessageLnVerb)
import Reader (Env (..), ReaderIO)
import System.IO (stderr)
import System.OsPath (OsPath)
import CGrep.Semantic.Tests (filterTests)
import Options (Options(..))
import Util (unquoteT)

lcKeywordsByType :: Map.Map FileType (HM.HashMap T.Text WordType)
lcKeywordsByType =
    let FileTypeInfoMap m = fileTypeInfoMap
     in Map.map (\fti -> HM.fromList [(T.toLower k, v) | (k, v) <- HM.toList (ftKeywords fti)]) m
{-# NOINLINE lcKeywordsByType #-}

search :: MVar () -> Maybe (FileType, FileTypeInfo) -> OsPath -> T.Text -> [T.Text] -> Bool -> ReaderIO [Match]
search lock info filename text patterns strict = do
    Env{..} <- ask
    let lindex = buildIndex text

    let filt = mkContextFilter opt ~! contextBitComment
        patFilt = contextFilterAll ~! contextBitComment
    let !contextFilter = mkContextFilterFn (fst <$> info) filt True
        !patContextFilter = mkContextFilterFn (fst <$> info) patFilt True

    let text'' = contextFilter text

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

    let patterns' = map (parseTokens pfilter (snd <$> info) strict . patContextFilter) patterns
        rawAtoms = map mkAtomsFromTokens patterns'
        patterns''
            | ignore_case opt =
                let lcKeywords = case info of
                        Nothing -> HM.empty
                        Just (ft, fti) ->
                            fromMaybe
                                (HM.fromList [(T.toLower k, v) | (k, v) <- HM.toList (ftKeywords fti)])
                                (Map.lookup ft lcKeywordsByType)
                 in if HM.null lcKeywords
                        then rawAtoms
                        else map (map (reclassifyAtom lcKeywords)) rawAtoms
            | otherwise = rawAtoms

    let matchers = concatMap extractAtomMatchers patterns''

    let !eligibleForSearch = edit_dist opt || textContainsOneOfOpt opt matchers text

    runSearch lindex filename eligibleForSearch $ do
        putMessageLnVerb 3 lock stderr $ "---\n" <> text'' <> "\n---"
        putMessageLnVerb 1 lock stderr $ "strategy  : running generic semantic search on " <> show filename
        putMessageLnVerb 2 lock stderr $ "atoms     : " <> show patterns''
        putMessageLnVerb 2 lock stderr $ "matchers  : " <> show matchers

        let indices' = if ignore_case opt || edit_dist opt then [] else textIndices matchers text''
            canSlice =
                isNothing (tests opt)
                    && not (ignore_case opt)
                    && not (edit_dist opt)
                    && all (all isNonEmptyExact) patterns''
                    && not (any (T.elem '\n') matchers)
            textToParse
                | canSlice = sliceToLastMatch matchers text''
                | otherwise = text''

        let tokens = parseTokens pfilter (snd <$> info) strict textToParse
        putMessageLnVerb 3 lock stderr $ "indices   : " <> show indices'

        -- get matching tokens ...

        let tokens' = case (fst <$> info) of
                        Nothing -> tokens
                        Just ft -> filterTests filename ft (tests opt) tokens

        putMessageLnVerb 3 lock stderr $ "tokens    : " <> show tokens
        putMessageLnVerb 3 lock stderr $ "tokens'   : " <> show tokens'

        let allMatches = sortBy (compare `on` tOffset) $ findAllMatches opt patterns'' tokens'

        -- convert Tokens to Chunks
        let matches = coerce allMatches :: [Chunk]
        putMessageLnVerb 2 lock stderr $ "matches   : " <> show matches

        mkMatches lindex filename text'' matches

isNonEmptyExact :: Atom -> Bool
isNonEmptyExact (Exact tok)
    | isTokenString tok = not (T.null ((unquoteT . trimT) (tToken tok)))
    | otherwise = not (T.null (tToken tok))
isNonEmptyExact _ = False
{-# INLINE isNonEmptyExact #-}

reclassifyAtom :: HM.HashMap T.Text WordType -> Atom -> Atom
reclassifyAtom lcKw = go
  where
    go (Exact tok)
        | isTokenIdentifier tok
        , Just wt <- HM.lookup (T.toLower (tToken tok)) lcKw =
            Exact $ case wt of
                Keyword -> mkTokenKeyword (tToken tok)
                NativeType -> mkTokenNativeType (tToken tok)
    go (Not a) = Not (go a)
    go (Or a b) = Or (go a) (go b)
    go a = a

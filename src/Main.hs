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

module Main (main) where

import Data.Char (toLower)

import Control.Monad (void, when)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))

import qualified Data.Map as M
import GHC.Conc (getNumCapabilities, setNumCapabilities)
import GHC.IO.Handle (hIsTerminalDevice)

import Options.Applicative (execParser)
import System.Environment (withArgs)
import System.Exit (exitSuccess)
import System.IO (stdin, stdout)

import CGrep.Common (trimT)
import CGrep.FileType (readKindList, readTypeList)
import CGrep.FileTypeMapTH (dumpFileTypeInfoMap, fileTypeInfoMap)
import CGrep.Parser.Atom (wildCardMap)
import CGrep.Search (isRegexp, startSearch)

import CmdOptions (parserInfo)
import Config (
    Config (configColors, configFileKinds, configFileTypes, configJobs),
    dumpPalette,
    getConfig,
 )
import Options (Options (..))

import Reader (Env (..))

import Control.Applicative (Alternative ((<|>)))
import Data.Functor (($>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List (union, (\\))
import qualified OsPath as OS
import System.OsPath (OsPath)
import qualified System.OsString as OS

main :: IO ()
main = do
    -- check whether this is a terminal device
    isTermIn <- hIsTerminalDevice stdin
    isTermOut <- hIsTerminalDevice stdout

    -- read config options
    (conf, _) <- getConfig

    -- read command-line options
    opt@Options{..} <-
        ( if isTermOut
                then \o -> o{color = color o || configColors conf}
                else id
            )
            <$> execParser parserInfo

    -- display lang-map and exit...
    when type_map $
        dumpFileTypeInfoMap fileTypeInfoMap >> exitSuccess

    -- display color palette and exit...
    when show_palette $
        dumpPalette >> exitSuccess

    -- check whether the pattern list is empty, display help message if it's the case
    when (null others && isTermIn && null file) $
        withArgs ["--help"] $
            void (execParser parserInfo)

    let others' = T.pack <$> others

    -- load patterns and filepaths:

    (patterns, paths) <- case file of
        Nothing -> do
            pure $ splitPatternsAndFiles others'
        Just f -> do
            readPatternsFromFile (OS.unsafeEncodeUtf f) >>= \ps -> pure (ps, OS.fromText <$> others')

    let patterns' = map (if ignore_case then ic else id) patterns
          where
            ic
                | (not . isRegexp) opt && semantic = T.unwords . map (\p -> if p `elem` (M.keys wildCardMap) then p else T.map toLower p) . T.words
                | otherwise = T.map toLower

    -- parse cmd line language list:
    let (l0, l1, l2) = readTypeList type_filter

    -- file type enabled:
    let types = (if null l0 then configFileTypes conf else l0 `union` l1) \\ l2
        kinds = if null kind_filter then configFileKinds conf else readKindList kind_filter

    -- specify number of cores
    cap <- case jobs <|> configJobs conf of
        (Just j) -> setNumCapabilities (j + 1) $> j
        Nothing -> getNumCapabilities

    -- run search
    runReaderT (startSearch paths patterns' types kinds isTermIn) (Env conf opt{jobs = Just cap})

readPatternsFromFile :: OsPath -> IO [T.Text]
readPatternsFromFile f
    | OS.null f = return []
    | otherwise = map trimT . T.lines <$> TIO.readFile (OS.toFilePath f)

splitPatternsAndFiles :: [T.Text] -> ([T.Text], [OsPath])
splitPatternsAndFiles [] = ([], [])
splitPatternsAndFiles [x] = ([x], [])
splitPatternsAndFiles (x : xs) = ([x], OS.fromText <$> xs)

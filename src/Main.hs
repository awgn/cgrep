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

module Main where

import qualified Codec.Binary.UTF8.String as UC
import qualified Data.ByteString.Char8 as C

import Data.Char (toLower)
import Data.List (elemIndex, genericLength, isInfixOf, isPrefixOf, isSuffixOf, nub, partition, sort, union, (\\))
import Data.Maybe (catMaybes)
import Data.Version (showVersion)

import Control.Monad (void, when)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)

import qualified Data.Map as M
import GHC.Conc (getNumCapabilities, setNumCapabilities)
import GHC.IO.Handle (hIsTerminalDevice)

import System.Console.CmdArgs (cmdArgsRun)
import System.Environment (withArgs)
import System.Exit (exitSuccess)
import System.IO (stderr, stdin, stdout)

import CGrep.Common (trim8)
import CGrep.FileType (readKindList, readTypeList)
import CGrep.FileTypeMap (dumpFileTypeInfoMap, fileTypeInfoMap)
import CGrep.Parser.Atom (wildCardMap)

import CmdOptions (options)
import Config (
    Config (configColors, configFileKinds, configFileTypes, configJobs),
    dumpPalette,
    getConfig,
 )
import Options (Options (..))
import Paths_cgrep (version)
import Verbose (putMsgLnVerbose)

import Reader (Env (..), ReaderIO)
import Search (isRegexp, parallelSearch)
import System.Posix.FilePath (RawFilePath)
import Util (partitionM)

import Control.Applicative (Alternative ((<|>)))
import Data.Functor (void, ($>))
import Data.List.Extra (notNull)

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
            <$> cmdArgsRun options

    -- check for multiple backends...
    when (length (catMaybes [if json then Just "" else Nothing]) > 1) $
        errorWithoutStackTrace "CGrep: you can use one back-end at time!"

    -- display lang-map and exit...
    when type_map $
        dumpFileTypeInfoMap fileTypeInfoMap >> exitSuccess

    -- display color palette and exit...
    when show_palette $
        dumpPalette >> exitSuccess

    -- check whether the pattern list is empty, display help message if it's the case
    when (null others && isTermIn && null file) $
        withArgs ["--help"] $
            void (cmdArgsRun options)

    let others' = C.pack <$> others

    -- load patterns
    patterns <-
        if null file
            then pure $ readPatternsFromCommandLine others'
            else readPatternsFromFile (C.pack file)

    let patterns' = map (if ignore_case then ic else id) patterns
          where
            ic
                | (not . isRegexp) opt && semantic = C.unwords . map (\p -> if p `elem` wildCardTokens then p else C.map toLower p) . C.words
                | otherwise = C.map toLower
              where
                wildCardTokens = "OR" : M.keys wildCardMap -- "OR" is not included in wildCardMap

    -- display the configuration in use

    -- when (isJust confpath) $
    --    hPutStrLn stderr $ showBold opt ("Using '" <> fromJust confpath <> "' configuration file...")

    -- load files to parse:
    let paths = getFilePaths (notNull file) others'

    -- parse cmd line language list:
    let (l0, l1, l2) = readTypeList type_filter

    -- file type enabled:
    let types = (if null l0 then configFileTypes conf else l0 `union` l1) \\ l2
        kinds = if null kind_filter then configFileKinds conf else readKindList kind_filter

    runReaderT
        ( do
            putMsgLnVerbose 1 stderr $ "cgrep " <> showVersion version <> "!"
            putMsgLnVerbose 1 stderr $ "File types: " <> show type_filter
            putMsgLnVerbose 1 stderr $ "File kinds: " <> show kinds
        )
        (Env conf opt)

    -- specify number of cores
    cap <- case jobs <|> configJobs conf of
        (Just j) -> setNumCapabilities (j + 1) $> j
        Nothing -> getNumCapabilities

    -- run search
    runReaderT (parallelSearch paths patterns' types kinds isTermIn) (Env conf opt{jobs = Just cap})

readPatternsFromFile :: RawFilePath -> IO [C.ByteString]
readPatternsFromFile "" = return []
readPatternsFromFile f = map trim8 . C.lines <$> C.readFile (C.unpack f)

readPatternsFromCommandLine :: [C.ByteString] -> [C.ByteString]
readPatternsFromCommandLine [] = []
readPatternsFromCommandLine xs
    | ":" `elem` xs = takeWhile (/= ":") xs
    | otherwise = [head xs]

getFilePaths :: Bool -> [RawFilePath] -> [RawFilePath]
getFilePaths False xs = case ":" `elemIndex` xs of
    Nothing -> if null xs then [] else tail xs
    (Just n) -> drop (n + 1) xs
getFilePaths True xs = xs
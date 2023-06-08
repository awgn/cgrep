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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified Data.ByteString.Char8 as C
import qualified Codec.Binary.UTF8.String as UC

import Data.List ( isSuffixOf, (\\), isInfixOf, nub, sort, union, isPrefixOf, genericLength, partition, elemIndex )
import Data.Maybe ( catMaybes )
import Data.Char ( toLower )
import Data.Version ( showVersion )

import Control.Monad.Trans.Reader ( ReaderT(runReaderT), ask )
import Control.Monad ( when, void )

import qualified Data.Map as M
import GHC.Conc ( getNumCapabilities, setNumCapabilities )
import GHC.IO.Handle ( hIsTerminalDevice )

import System.IO ( stdin, stdout, stderr )
import System.Console.CmdArgs ( cmdArgsRun )
import System.Exit ( exitSuccess )
import System.Environment ( withArgs )

import CGrep.LanguagesMap ( dumpLanguagesMap, languagesMap )
import CGrep.Parser.Atom ( wildCardMap )
import CGrep.Language ( splitLanguagesList )
import CGrep.Common ( trim8 )

import Verbose ( putMsgLnVerbose )
import Paths_cgrep ( version )
import CmdOptions ( options )
import Options ( Options(..) )
import Config
    ( dumpPalette, getConfig, Config(configLanguages, configColors, configJobs) )
import Util ( partitionM)
import Reader ( ReaderIO, Env (..) )
import Search ( parallelSearch, isRegexp )
import System.Posix.FilePath (RawFilePath)

import Data.List.Extra (notNull)
import Data.Functor
import Control.Applicative

main :: IO ()
main = do
    -- check whether this is a terminal device
    isTermIn  <- hIsTerminalDevice stdin
    isTermOut <- hIsTerminalDevice stdout

    -- read config options
    (conf, _) <- getConfig

    -- read command-line options
    opt@Options{..} <- (if isTermOut
                then \o -> o { color = color o || configColors conf }
                else id) <$> cmdArgsRun options

    -- check for multiple backends...
    when (length (catMaybes [ if json then Just "" else Nothing ]) > 1) $
        error "Cgrep: you can use one back-end at time!"

    -- display lang-map and exit...
    when language_map $
        dumpLanguagesMap languagesMap >> exitSuccess

    -- display color palette and exit...
    when show_palette $
        dumpPalette >> exitSuccess

    -- check whether the pattern list is empty, display help message if it's the case
    when (null others && isTermIn && null file) $
        withArgs ["--help"] $ void (cmdArgsRun options)

    let others' = C.pack <$> others

    -- load patterns
    patterns <- if null file then pure $ readPatternsFromCommandLine others'
                             else readPatternsFromFile (C.pack file)

    let patterns' = map (if ignore_case then ic else id) patterns
            where ic | (not . isRegexp) opt && semantic = C.unwords . map (\p -> if p `elem` wildCardTokens then p else C.map toLower p) . C.words
                     | otherwise = C.map toLower
                        where wildCardTokens = "OR" : M.keys wildCardMap   -- "OR" is not included in wildCardMap

    -- display the configuration in use

    -- when (isJust confpath) $
    --    hPutStrLn stderr $ showBold opt ("Using '" <> fromJust confpath <> "' configuration file...")

    -- load files to parse:
    let paths = getFilePaths (notNull file) others'

    -- parse cmd line language list:
    let (l0, l1, l2) = splitLanguagesList language_filter

    -- language enabled:
    let langs = (if null l0 then configLanguages conf else l0 `union` l1) \\ l2

    runReaderT (do putMsgLnVerbose 1 stderr $ "cgrep " <> showVersion version <> "!"
        ) (Env conf opt)

    -- specify number of cores
    cap <- case jobs <|> configJobs conf of
            (Just j) ->  setNumCapabilities j $> j
            Nothing  ->  getNumCapabilities

    -- run search
    runReaderT (parallelSearch paths patterns' langs isTermIn) (Env conf opt {jobs = Just cap})


readPatternsFromFile :: RawFilePath -> IO [C.ByteString]
readPatternsFromFile "" = return []
readPatternsFromFile f  = map trim8 . C.lines <$> C.readFile (C.unpack f)


readPatternsFromCommandLine :: [C.ByteString] -> [C.ByteString]
readPatternsFromCommandLine [] = []
readPatternsFromCommandLine xs | ":" `elem` xs = takeWhile (/= ":") xs
                               | otherwise = [ head xs ]


getFilePaths :: Bool -> [RawFilePath] -> [RawFilePath]
getFilePaths False xs = case ":" `elemIndex` xs of
    Nothing  -> if null xs then [] else tail xs
    (Just n) -> drop (n+1)  xs
getFilePaths True xs = xs
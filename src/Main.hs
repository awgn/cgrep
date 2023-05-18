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
import GHC.Conc ( getNumCapabilities )
import GHC.IO.Handle ( hIsTerminalDevice )

import System.IO ( stdin, stdout )
import System.Console.CmdArgs ( cmdArgsRun )
import System.Exit ( exitSuccess )
import System.Environment ( withArgs )

import CGrep.LanguagesMap ( dumpLanguagesMap, languagesMap )
import CGrep.Search ( isRegexp )
import CGrep.Parser.Atom ( wildCardMap )
import CGrep.Language ( splitLanguagesList )
import CGrep.Common ( trim8 )

import Verbose ( putStrLnVerbose )
import Paths_cgrep ( version )
import CmdOptions ( options )
import Options ( Options(..) )
import Config
    ( dumpPalette, getConfig, Config(configLanguages, configColors) )
import Util ( partitionM, notNull )
import Reader ( OptionIO, Env (..) )
import Search ( parallelSearch )


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

    -- load patterns
    patterns <- if null file then pure $ readPatternsFromCommandLine others
                             else readPatternsFromFile file

    let patterns' = map (if ignore_case then ic else id) patterns
            where ic | (not . isRegexp) opt && semantic = C.unwords . map (\p -> if p `elem` wildCardTokens then p else C.map toLower p) . C.words
                     | otherwise = C.map toLower
                        where wildCardTokens = "OR" : M.keys wildCardMap   -- "OR" is not included in wildCardMap

    -- display the configuration in use

    -- when (isJust confpath) $
    --    hPutStrLn stderr $ showBold opt ("Using '" <> fromJust confpath <> "' configuration file...")

    -- load files to parse:
    let paths = getFilePaths (notNull file) others

    -- parse cmd line language list:
    let (l0, l1, l2) = splitLanguagesList language_filter

    -- language enabled:
    let langs = (if null l0 then configLanguages conf else l0 `union` l1) \\ l2

    runReaderT (do putStrLnVerbose 1 $ "Cgrep " <> showVersion version <> "!"
                   putStrLnVerbose 1 $ "patterns  : " <> show patterns'
                   putStrLnVerbose 1 $ "files     : " <> show paths
        ) (Env conf opt Nothing Nothing)

    -- specify number of cores
    njobs <- if jobs /= 0
                then return jobs
                else getNumCapabilities

    -- run search
    runReaderT (parallelSearch paths patterns' langs isTermIn) (Env conf opt { jobs = njobs} Nothing Nothing)


readPatternsFromFile :: FilePath -> IO [C.ByteString]
readPatternsFromFile "" = return []
readPatternsFromFile f  = map trim8 . C.lines <$> C.readFile f

readPatternsFromCommandLine :: [String] -> [C.ByteString]
readPatternsFromCommandLine [] = []
readPatternsFromCommandLine xs | ":" `elem` xs = C.pack . UC.encodeString <$> takeWhile (/= ":") xs
                               | otherwise = [ (C.pack . UC.encodeString) (head xs) ]

getFilePaths :: Bool        ->     -- pattern(s) from file
                [String]    ->     -- list of patterns and files
                [String]
getFilePaths False xs = case ":" `elemIndex` xs of
    Nothing  -> if null xs then [] else tail xs
    (Just n) -> drop (n+1)  xs
getFilePaths True xs = xs
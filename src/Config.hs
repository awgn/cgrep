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
{-# LANGUAGE DeriveGeneric #-}

module Config (
    Config (..),
    dumpPalette,
    getConfig,
) where

import Control.Monad (MonadPlus (mzero), filterM, forM_)
import System.Console.ANSI (
    Color (Blue, Cyan, Green, Magenta, Red, White, Yellow),
    ColorIntensity (Vivid),
    ConsoleIntensity (BoldIntensity),
    ConsoleLayer (Foreground),
    SGR (SetColor, SetConsoleIntensity),
    setSGRCode,
 )
import System.Directory (doesFileExist, getHomeDirectory)

import System.Console.ANSI.Types (
    Color (Blue, Cyan, Green, Magenta, Red, White, Yellow),
    ColorIntensity (Vivid),
    ConsoleIntensity (BoldIntensity),
    ConsoleLayer (Foreground),
    SGR (SetColor, SetConsoleIntensity, SetPaletteColor),
    xterm6LevelRGB,
 )

import Data.Aeson (FromJSON (parseJSON), (.!=), (.:?))
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Yaml as Y

import CGrep.FileType (FileType)
import GHC.Generics (Generic)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List.Split (splitOn)
import System.FilePath ((</>))

import CGrep.FileKind (FileKind)
import Data.List.Extra (notNull)
import Text.Read (readMaybe)
import System.OsPath (OsPath)

cgreprc :: FilePath
cgreprc = "cgreprc"

data Config = Config
    { configFileTypes :: [FileType]
    , configFileKinds :: [FileKind]
    , configPruneDirs :: [String]
    , configColors :: Bool
    , configColorFile :: [SGR]
    , configColorMatch :: [SGR]
    , configFileLine :: Bool
    , configJobs :: Maybe Int
    }
    deriving stock (Show, Read)

defaultConfig :: Config
defaultConfig =
    Config
        { configFileTypes = []
        , configFileKinds = []
        , configPruneDirs = []
        , configColors = False
        , configColorFile = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
        , configColorMatch = [SetConsoleIntensity BoldIntensity]
        , configFileLine = False
        , configJobs = Nothing
        }

mkConfig :: YamlConfig -> Config
mkConfig YamlConfig{..} =
    let configFileTypes = mapMaybe readMaybe yamlFileTypes
        configFileKinds = mapMaybe readMaybe yamlFileKinds
        configPruneDirs = yamlPruneDirs
        configColors = yamlColors
        configColorFile = fromMaybe [] (yamlColorFileName >>= readColor)
        configColorMatch = fromMaybe [] (yamlColorMatch >>= readColor)
        configFileLine = yamlFileLine
        configJobs = yamlJobs
     in Config{..}

data YamlConfig = YamlConfig
    { yamlFileTypes :: [String]
    , yamlFileKinds :: [String]
    , yamlPruneDirs :: [String]
    , yamlColors :: Bool
    , yamlColorFileName :: Maybe String
    , yamlColorMatch :: Maybe String
    , yamlFileLine :: Bool
    , yamlJobs :: Maybe Int
    }
    deriving stock (Show, Generic)

instance Y.FromJSON YamlConfig where
    parseJSON (Y.Object v) =
        YamlConfig
            <$> v .:? "file_types" .!= []
            <*> v .:? "file_kinds" .!= []
            <*> v .:? "prune_dirs" .!= []
            <*> v .:? "colors" .!= False
            <*> v .:? "color_filename" .!= Nothing
            <*> v .:? "color_match" .!= Nothing
            <*> v .:? "file_line" .!= False
            <*> v .:? "threads" .!= Nothing
    parseJSON _ = mzero

getConfig :: IO (Config, Maybe FilePath)
getConfig = do
    home <- getHomeDirectory
    confs <- filterM doesFileExist [cgreprc, "." <> cgreprc, home </> "." <> cgreprc, "/etc" </> cgreprc]
    if notNull confs
        then do
            conf <- Y.decodeFileEither (head confs)
            case conf of
                Left e -> errorWithoutStackTrace $ "CGrep:" <> Y.prettyPrintParseException e
                Right yconf -> return (mkConfig yconf, Just (head confs))
        else return (defaultConfig, Nothing)

readColor :: String -> Maybe [SGR]
readColor "Bold" = Just [SetConsoleIntensity BoldIntensity]
readColor "Red" = Just [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
readColor "Green" = Just [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
readColor "Yellow" = Just [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
readColor "Blue" = Just [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
readColor "Magenta" = Just [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Magenta]
readColor "Cyan" = Just [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]
readColor "White" = Just [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
readColor "Orange" = Just [SetConsoleIntensity BoldIntensity, SetPaletteColor Foreground $ xterm6LevelRGB 5 2 0]
readColor "Acqua" = Just [SetConsoleIntensity BoldIntensity, SetPaletteColor Foreground $ xterm6LevelRGB 2 5 4]
readColor xs = case splitOn ":" xs of
    [r, g, b] -> Just [SetConsoleIntensity BoldIntensity, SetPaletteColor Foreground $ xterm6LevelRGB (read r) (read g) (read b)]
    _ -> Nothing

dumpPalette :: IO ()
dumpPalette = do
    let palette = [(r, g, b) | r <- [0 .. 5], g <- [0 .. 5], b <- [0 .. 5]]
    forM_ palette $ \(r, g, b) -> do
        putStrLn $ setSGRCode [SetConsoleIntensity BoldIntensity, SetPaletteColor Foreground $ xterm6LevelRGB r g b] <> "COLOR " <> show r <> ":" <> show g <> ":" <> show b <> setSGRCode []

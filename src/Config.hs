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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Monad ( MonadPlus(mzero), filterM, forM_ )
import System.Directory ( doesFileExist, getHomeDirectory )
import System.FilePath ((</>))
import System.Console.ANSI
    ( Color(White, Red, Green, Yellow, Blue, Magenta, Cyan),
      ColorIntensity(Vivid),
      ConsoleIntensity(BoldIntensity),
      ConsoleLayer(Foreground),
      SGR(SetColor, SetConsoleIntensity), setSGRCode )

import System.Console.ANSI.Types
    ( SGR(SetPaletteColor, SetColor, SetConsoleIntensity),
      xterm6LevelRGB,
      Color(White, Red, Green, Yellow, Blue, Magenta, Cyan),
      ColorIntensity(Vivid),
      ConsoleIntensity(BoldIntensity),
      ConsoleLayer(Foreground) )

import qualified Data.Yaml  as Y
import Data.Aeson ( (.!=), (.:?), FromJSON(parseJSON) )
import Data.Maybe ( fromMaybe, mapMaybe )

import GHC.Generics ( Generic )
import CGrep.Language ( Language )
import Util ( notNull, readMaybe )

import Data.List.Split
import qualified Data.ByteString as B

cgreprc :: FilePath
cgreprc = "cgreprc"


data Config = Config
  {   configLanguages  :: [Language]
  ,   configPruneDirs  :: [String]
  ,   configColors     :: Bool
  ,   configColorFile  :: [SGR]
  ,   configColorMatch :: [SGR]
  ,   configFileLine   :: Bool
  } deriving (Show, Read)


defaultConfig :: Config
defaultConfig = Config
  {   configLanguages   = []
  ,   configPruneDirs   = []
  ,   configColors      = False
  ,   configColorFile   = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
  ,   configColorMatch  = [SetConsoleIntensity BoldIntensity]
  ,   configFileLine    = False
  }


mkConfig :: YamlConfig -> Config
mkConfig YamlConfig{..} =
   let configLanguages  = mapMaybe readMaybe yamlLanguages
       configPruneDirs  = yamlPruneDirs
       configColors     = yamlColors
       configColorFile  = fromMaybe [] (yamlColorFileName >>= readColor)
       configColorMatch = fromMaybe [] (yamlColorMatch >>= readColor)
       configFileLine   = yamlFileLine
    in Config {..}


data YamlConfig = YamlConfig
  {   yamlLanguages     :: [String]
  ,   yamlPruneDirs     :: [String]
  ,   yamlColors        :: Bool
  ,   yamlColorFileName :: Maybe String
  ,   yamlColorMatch    :: Maybe String
  ,   yamlFileLine      :: Bool
  } deriving (Show, Generic)


instance Y.FromJSON YamlConfig where
 parseJSON (Y.Object v) =
    YamlConfig <$> v .:? "languages"        .!= []
               <*> v .:? "prune_dirs"       .!= []
               <*> v .:? "colors"           .!= False
               <*> v .:? "color_filename"   .!= Nothing
               <*> v .:? "color_match"      .!= Nothing
               <*> v .:? "file_line"        .!= False
 parseJSON _ = mzero


getConfig :: IO (Config, Maybe FilePath)
getConfig = do
    home  <- getHomeDirectory
    confs <- filterM doesFileExist [cgreprc, "." ++ cgreprc, home </> "." ++ cgreprc, "/etc" </> cgreprc]
    if notNull confs
        then do
            conf <- Y.decodeFileEither (head confs)
            case conf of
                Left  e -> errorWithoutStackTrace $ Y.prettyPrintParseException e
                Right yconf -> return (mkConfig yconf, Just (head confs))
        else return (defaultConfig, Nothing)


readColor :: String -> Maybe [SGR]
readColor "Bold"      =  Just [SetConsoleIntensity BoldIntensity]
readColor "Red"       =  Just [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
readColor "Green"     =  Just [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
readColor "Yellow"    =  Just [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
readColor "Blue"      =  Just [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
readColor "Magenta"   =  Just [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Magenta]
readColor "Cyan"      =  Just [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]
readColor "White"     =  Just [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
readColor "Orange"    =  Just [SetConsoleIntensity BoldIntensity, SetPaletteColor Foreground $ xterm6LevelRGB 5 2 0]
readColor "Acqua"     =  Just [SetConsoleIntensity BoldIntensity, SetPaletteColor Foreground $ xterm6LevelRGB 2 5 4]
readColor xs          = case splitOn ":" xs of
                          [r, g, b] -> Just [SetConsoleIntensity BoldIntensity, SetPaletteColor Foreground $ xterm6LevelRGB (read r) (read g) (read b)]
                          _      -> Nothing


dumpPalette :: IO ()
dumpPalette = do
  let palette = [(r, g, b) | r <- [0..5], g <- [0..5], b <- [0..5]]
  forM_ palette $ \(r, g, b) -> do
    putStrLn $ setSGRCode [SetConsoleIntensity BoldIntensity, SetPaletteColor Foreground $ xterm6LevelRGB r g b] <> "COLOR " <> show r <> ":" <> show g <> ":" <> show b <> setSGRCode []
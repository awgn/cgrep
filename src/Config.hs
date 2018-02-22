--
-- Copyright (c) 2013 Bonelli Nicola <bonelli@antifork.org>
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

import Data.List
import Data.Char

import Control.Monad
import System.Directory
import System.FilePath ((</>))
import System.Console.ANSI
import Data.Semigroup ((<>), Semigroup(..))

import qualified Data.Yaml  as Y
import Data.Aeson
import Data.Maybe

import GHC.Generics
import CGrep.Lang
import Util

cgreprc :: FilePath
cgreprc = "cgreprc"


data Config = Config
  {   configLanguages  :: [Lang]
  ,   configPruneDirs  :: [String]
  ,   configColors     :: Bool
  ,   configColorFile  :: [SGR]
  ,   configColorMatch :: [SGR]
  } deriving (Show, Read)


defaultConfig :: Config
defaultConfig = Config
  {   configLanguages   = []
  ,   configPruneDirs   = []
  ,   configColors      = False
  ,   configColorFile   = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
  ,   configColorMatch  = [SetConsoleIntensity BoldIntensity]
  }


mkConfig :: YamlConfig -> Config
mkConfig YamlConfig{..} =
   let configLanguages  = mapMaybe readMaybe yamlLanguages
       configPruneDirs  = yamlPruneDirs
       configColors     = yamlColors
       configColorFile  = fromMaybe [] (yamlColorFileName >>= readColor)
       configColorMatch = fromMaybe [] (yamlColorMatch >>= readColor)
    in Config {..}


data YamlConfig = YamlConfig
  {   yamlLanguages     :: [String]
  ,   yamlPruneDirs     :: [String]
  ,   yamlColors        :: Bool
  ,   yamlColorFileName :: Maybe String
  ,   yamlColorMatch    :: Maybe String
  } deriving (Show, Generic)


instance Y.FromJSON YamlConfig where
 parseJSON (Y.Object v) =
    YamlConfig <$> v .:? "languages"        .!= []
               <*> v .:? "prune_dirs"       .!= []
               <*> v .:? "colors"           .!= False
               <*> v .:? "color_filename"   .!= Nothing
               <*> v .:? "color_match"      .!= Nothing


getConfig :: IO (Config, Maybe FilePath)
getConfig = do
    home  <- getHomeDirectory
    confs <- filterM doesFileExist [cgreprc, "." ++ cgreprc, home </> "." ++ cgreprc, "/etc" </> cgreprc]
    if notNull confs
        then do
            conf <- Y.decodeFileEither (head confs)
            print conf
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
readColor _           =  Nothing





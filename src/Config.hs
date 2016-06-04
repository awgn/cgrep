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

module Config where

import Data.List
import Data.Char

import Control.Monad
import System.Directory
import System.FilePath ((</>))
import System.Console.ANSI

import Util
import CGrep.Lang


cgreprc :: FilePath
cgreprc = "cgreprc"


data Config = Config
  {   configLanguages  :: [Lang]
  ,   configPruneDirs  :: [String]
  ,   configAutoColor  :: Bool
  ,   configColorFile  :: [SGR]
  ,   configColorMatch :: [SGR]
  } deriving (Show, Read)


defaultConfig :: Config

defaultConfig = Config
  {   configLanguages   = []
  ,   configPruneDirs   = []
  ,   configAutoColor   = False
  ,   configColorFile   = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
  ,   configColorMatch  = [SetConsoleIntensity BoldIntensity]
  }


dropComments :: String -> String
dropComments =  unlines . filter notComment . lines
    where notComment = (not . ("#" `isPrefixOf`)) . dropWhile isSpace


getConfig :: IO (Config, Maybe FilePath)
getConfig = do
    home  <- getHomeDirectory
    confs <- filterM doesFileExist [cgreprc, "." ++ cgreprc, home </> "." ++ cgreprc, "/etc" </> cgreprc]
    if notNull confs
        then liftM dropComments (readFile (head confs)) >>= \xs ->
              return (prettyRead xs "Config error" :: Config, Just (head confs))
        else return (defaultConfig, Nothing)



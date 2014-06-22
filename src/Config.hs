--
-- Copyright (c) 2013 Bonelli Nicola <bonelli@antifork.org>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the StopNerds Public License as published by
-- the StopNerds Foundation; either version 1 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- StopNerds Public License for more details.
--
-- You should have received a copy of the StopNerds Public License
-- along with this program; if not, see <http://stopnerds.org/license/>.
--

module Config where

import Data.Maybe
import Control.Monad
import System.Directory
import System.FilePath ((</>))

import Util
import CGrep.Lang


cgreprc :: FilePath
cgreprc = "cgreprc"

version :: String
version = "6.4.3"

data Config = Config
              {
                    languages :: [Lang],
                    pruneDirs :: [String]

              } deriving (Show, Read)


getConfig :: IO Config
getConfig = do
    home <- getHomeDirectory
    conf <- liftM msum $ forM [ home </> "." ++ cgreprc, "/etc" </> cgreprc ] $ \f ->
                doesFileExist f >>= \b -> return $ guard b >> Just f

    if isJust conf then readFile (fromJust conf) >>= \xs ->
                        return (prettyRead (dropComments xs) "Config error" :: Config)
                   else return $ Config [] []

    where dropComments :: String -> String
          dropComments = unlines . map (takeWhile $ not .(== '#')) . lines



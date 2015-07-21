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

import Data.Maybe
import Control.Monad
import System.Directory
import System.FilePath ((</>))

import Util
import CGrep.Lang


cgreprc :: FilePath
cgreprc = "cgreprc"

version :: String
version = "6.4.20"


data Config = Config
              {
                    configLanguages  :: [Lang],
                    configPruneDirs  :: [String],
                    configAutoColor  :: Bool

              } deriving (Show, Read)



getConfig :: IO Config
getConfig = do
    home <- getHomeDirectory
    conf <- liftM msum $ forM [ home </> "." ++ cgreprc, "/etc" </> cgreprc ] $ \f ->
                (doesFileExist >=> (\b -> return $ guard b >> Just f)) f

    if isJust conf then readFile (fromJust conf) >>= \xs ->
                        return (prettyRead (dropComments xs) "Config error" :: Config)
                   else return $ Config [] [] False

    where dropComments :: String -> String
          dropComments = unlines . map (takeWhile $ not .(== '#')) . lines



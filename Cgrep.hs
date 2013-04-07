module Cgrep where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad(liftM)
import Options

import CgrepFunction
import CgrepSimple


cgrep :: Options -> CgrepFunction

cgrep Options { word = False, regex = False, code = False, comment = False, string = False } = cgrepSimpleLine
cgrep Options { word = True,  regex = False, code = False, comment = False, string = False } = cgrepSimpleToken
cgrep _ = undefined



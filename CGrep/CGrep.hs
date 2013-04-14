module CGrep.CGrep where

import CGrep.Options
import CGrep.Function
import CGrep.Simple


cgrep :: Options -> CgrepFunction

cgrep Options { word = False, regex = False, code = False, comment = False, string = False } = cgrepSimpleLine
cgrep Options { word = True,  regex = False, code = False, comment = False, string = False } = cgrepSimpleToken
cgrep _ = undefined



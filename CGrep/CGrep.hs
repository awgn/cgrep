module CGrep.CGrep where

import CGrep.Options
import CGrep.Function

import CGrep.SimpleLines
import CGrep.SimpleWords


cgrep :: Options -> CgrepFunction

cgrep Options { word = False, regex = False, code = False, comment = False, string = False } = cgrepSimpleLines
cgrep Options { word = True,  regex = False, code = False, comment = False, string = False } = cgrepSimpleWords
cgrep _ = undefined



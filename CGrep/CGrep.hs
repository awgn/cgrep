module CGrep.CGrep where

import CGrep.Options
import CGrep.Function

import CGrep.SimpleLines
import CGrep.SimpleWords
import CGrep.BoyerMoore
import CGrep.CppFilter


cgrep :: Options -> CgrepFunction

cgrep Options { word = True,  regex = False, code = False, comment = False, string = False, boyer_moore = False } = cgrepSimpleWords
cgrep Options { word = False, regex = False, code = False, comment = False, string = False, boyer_moore = False } = cgrepSimpleLines
cgrep Options { word = False, regex = False, code = False, comment = False, string = False, boyer_moore = True  } = cgrepBoyerMoore
cgrep Options { regex = False } = cgrepCppFilter

cgrep _ = undefined


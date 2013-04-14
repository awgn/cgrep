module CGrep.CGrep where

import CGrep.Options
import CGrep.Function

import CGrep.BoyerMoore
import CGrep.CppFilter
import CGrep.CppTokenizer
import CGrep.Simple


cgrep :: Options -> CgrepFunction

cgrep Options { regex = False, code = False, comment = False, string = False, tokens = [], boyer_moore = False } = cgrepSimple
cgrep Options { regex = False, code = False, comment = False, string = False, tokens = [], boyer_moore = True  } = cgrepBoyerMoore
cgrep Options { regex = False, tokens = [] } = cgrepCppFilter
cgrep Options { regex = False } = cgrepCppTokenizer

cgrep _ = undefined



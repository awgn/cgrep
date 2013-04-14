module CGrep.CGrep where

import CGrep.Options
import CGrep.Function

import CGrep.BoyerMoore
import CGrep.CppFilter
import CGrep.Simple


cgrep :: Options -> CgrepFunction

cgrep Options { regex = False, code = False, comment = False, string = False, boyer_moore = False } = cgrepSimple
cgrep Options { regex = False, code = False, comment = False, string = False, boyer_moore = True  } = cgrepBoyerMoore
cgrep Options { regex = False } = cgrepCppFilter

cgrep _ = undefined



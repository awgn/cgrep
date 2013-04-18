module CGrep.CGrep where

import CGrep.Options
import CGrep.Function

import CGrep.Strategy.Simple
import CGrep.Strategy.Context
import CGrep.Strategy.Tokenizer


cgrep :: Options -> CgrepFunction

cgrep Options { regex = False, code = False, comment = False, literal = False, identifier = False, keyword = False, directive = False, header = False, number = False, string = False, char = False, oper = False } = cgrepSimple
cgrep Options { regex = False, identifier = False, keyword = False, directive = False, header = False, number = False, string = False, char = False, oper = False } = cgrepCppContext
cgrep Options { regex = False } = cgrepCppTokenizer

cgrep _ = undefined



module CGrep.CppTokenizer (cgrepCppTokenizer) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import CGrep.Function
import CGrep.Output
import CGrep.Options 

import Control.Monad (when)

import qualified CGrep.Cpp.Filter as Cpp
import qualified CGrep.Cpp.Token  as Cpp

import Data.List

cgrepCppTokenizer :: CgrepFunction
cgrepCppTokenizer opt ps f = do
    src <- lazyReadFile f

    let source   = Cpp.filter (mkContextFilter opt) src
    let tks      = filter (Cpp.tokenFilter $ Cpp.TokenFilter { Cpp.filtIdentifier = identifier opt, 
                                                               Cpp.filtDirective  = directive opt,
                                                               Cpp.filtKeyword    = keyword opt,
                                                               Cpp.filtHeader     = header opt, 
                                                               Cpp.filtString     = string opt,
                                                               Cpp.filtNumber     = number opt,
                                                               Cpp.filtChar       = char opt,
                                                               Cpp.filtOper       = oper opt}) (Cpp.tokens source)

    when (debug opt) $ print tks 

    let content  = LC.lines source
    let tks_res  = simpleTokenGrep opt f lps tks 

    return $ map (\t -> let ln = fromIntegral (Cpp.lineno t) in LazyOutput f (ln+1) (content !! ln) [] ) tks_res
        where lps = map C.unpack ps


mkContextFilter :: Options -> Cpp.ContextFilter
mkContextFilter opt = if not (code opt && comment opt && literal opt) 
                       then Cpp.ContextFilter { Cpp.getCode = True,     Cpp.getComment = False, Cpp.getLiteral = True }
                       else Cpp.ContextFilter { Cpp.getCode = code opt, Cpp.getComment = False, Cpp.getLiteral = literal opt }


simpleTokenGrep :: Options -> FilePath -> [String] -> [Cpp.Token] -> [Cpp.Token]
simpleTokenGrep opt _ ps = filter (\tok -> (let heystack = Cpp.toString tok in any (`isInfixOf` heystack) ps) `xor` invert_match opt) 




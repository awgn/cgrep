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

module CGrep.Strategy.Tokenizer (cgrepCppTokenizer) where

import qualified Data.ByteString.Char8 as C
-- import qualified Data.ByteString.Lazy.Char8 as LC

import CGrep.Function
import CGrep.Output
import CGrep.StringLike
import CGrep.Filter 
import CGrep.Lang
import CGrep.Common

import Control.Monad (when)
-- import Debug.Trace

import Data.List
import Data.Function

import Options 
import Util

import qualified CGrep.Strategy.Cpp.Token  as Cpp


cgrepCppTokenizer :: CgrepFunction
cgrepCppTokenizer opt ps f = do

    source <- if f == "" then slGetContents (ignore_case opt)  
                            else slReadFile (ignore_case opt) f

    let filtered = filterContext (lookupLang f) (mkContextFilter opt) source
    
    -- parse source code, get the Cpp.Token list...
    --
    
    let all_ts = Cpp.tokenizer filtered

    let ts = filter (Cpp.tokenFilter Cpp.TokenFilter { Cpp.filtIdentifier = identifier opt, 
                                                       Cpp.filtDirective  = directive opt,
                                                       Cpp.filtKeyword    = keyword opt,
                                                       Cpp.filtHeader     = header opt, 
                                                       Cpp.filtString     = string opt,
                                                       Cpp.filtNumber     = number opt,
                                                       Cpp.filtChar       = char opt,
                                                       Cpp.filtOper       = oper opt}) all_ts

    let content  = C.lines source

    let ts_res  = if snippet opt 
                     then sortBy (compare `on` Cpp.offset) $ nub $ simpleTokenMatch opt f lpt all_ts 
                     else simpleTokenGrep  opt f lps ts
    
    when (debug opt) $ do 
        C.putStrLn filtered
        print opt
        print lpt
        print ts_res

    return $ nubBy outputEqual $ map (\t -> let ln = fromIntegral (Cpp.lineno t) in Output f (ln+1) (content !! ln) [] ) ts_res
        where lps = map C.unpack ps
              lpt = map (Cpp.tokenizer . filterContext (Just Cpp) sourceCodeFilter) ps 
              outputEqual (Output f' n' _ _) (Output f'' n'' _ _) = (f' == f'') && (n' == n'') 

mkContextFilter :: Options -> ContextFilter
mkContextFilter opt = if not (code opt && comment opt && literal opt) 
                       then ContextFilter { getCode = True,     getComment = False, getLiteral = True }
                       else ContextFilter { getCode = code opt, getComment = False, getLiteral = literal opt }


sourceCodeFilter :: ContextFilter 
sourceCodeFilter = ContextFilter { getCode = True, getComment = False, getLiteral = True }   


simpleTokenGrep :: Options -> FilePath -> [String] -> [Cpp.Token] -> [Cpp.Token]
simpleTokenGrep opt _ ps = filter (notNull . slGrep (word opt) (invert_match opt) ps . Cpp.toString) 


simpleTokenMatch :: Options -> FilePath -> [[Cpp.Token]] -> [Cpp.Token] -> [Cpp.Token]
-- simpleTokenMatch opt _ ps | trace ("ps = " ++ (show ps)) False = undefined 
simpleTokenMatch _ _ [] _ = []
simpleTokenMatch opt f (g:gs) ts = map (ts !!) (findIndices (compareGroup (word opt) (invert_match opt) g) tokenGroups) ++ simpleTokenMatch opt f gs ts 
    where tokenGroups = spanGroup (length g) ts


type WordMatch   = Bool
type InvertMatch = Bool


compareGroup :: WordMatch -> InvertMatch -> [Cpp.Token] -> [Cpp.Token] -> Bool
compareGroup wordmatch invert l r =  all (uncurry (compareToken wordmatch invert)) (zip l r) 

compareToken :: WordMatch -> InvertMatch -> Cpp.Token -> Cpp.Token -> Bool
-- compareToken True invert (Cpp.TIdentifier {}) (Cpp.TIdentifier { Cpp.toString = "_" })                  = invert `xor` True 
compareToken True invert (Cpp.TIdentifier { Cpp.toString = "_" }) (Cpp.TIdentifier {})                  = invert `xor` True 
compareToken True invert (Cpp.TIdentifier { Cpp.toString = l }) (Cpp.TIdentifier { Cpp.toString = r })  = invert `xor` (l == r)  
compareToken True invert l r                                                                            = invert `xor` (Cpp.toString l == Cpp.toString r)

-- compareToken False invert (Cpp.TIdentifier {}) (Cpp.TIdentifier { Cpp.toString = "_" })                 = invert `xor` True
compareToken False invert (Cpp.TIdentifier { Cpp.toString = "_" }) (Cpp.TIdentifier {})                 = invert `xor` True
compareToken False invert (Cpp.TIdentifier { Cpp.toString = l }) (Cpp.TIdentifier { Cpp.toString = r }) = invert `xor` (l `isInfixOf` r)
compareToken False invert l r                                                                           = invert `xor` (Cpp.toString l `isInfixOf` Cpp.toString r)



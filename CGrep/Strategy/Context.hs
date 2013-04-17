module CGrep.Strategy.Context (cgrepCppFilter) where

import Data.ByteString.Lazy.Search as LC

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import CGrep.Function
import CGrep.Output
import CGrep.Options 

import CGrep.StringLike

import Control.Monad (when)

import qualified CGrep.Cpp.Filter as Cpp

cgrepCppFilter :: CgrepFunction
cgrepCppFilter opt ps f = do
    source <- lazyReadFile f
    
    let filtered =  Cpp.filter Cpp.ContextFilter { Cpp.getCode = code opt, Cpp.getComment = comment opt, Cpp.getLiteral = literal opt } source
    let content = zip [1..] $ LC.lines filtered

    when (debug opt) $ print content

    return $ concatMap (if word opt then simpleWordGrep opt f lps
                                    else simpleLineGrep opt f  ps) content
        where lps = map (LC.fromChunks . (:[])) ps



simpleLineGrep :: Options -> FilePath -> [C.ByteString] -> (Int, LC.ByteString) -> [Output]
simpleLineGrep opt f ps (n, l) = 
   if null tks `xor` invert_match opt 
     then []
     else [LazyOutput f n l (map C.unpack tks)]
   where tks = filter (`sumMatch` l) ps   
         sumMatch = if ignore_case opt 
                      then (\p q -> (LC.fromChunks . (:[])) p `ciIsInfixOf` q) 
                      else (\p q -> not . null $ LC.indices p q)


simpleWordGrep :: Options -> FilePath -> [LC.ByteString] -> (Int, LC.ByteString) -> [Output]
simpleWordGrep opt f ps (n, l) = 
   if null tks `xor` invert_match opt 
     then []
     else [LazyOutput f n l (map LC.unpack tks)]
   where tks = filter (`match` gwords l) ps   
         match a = if ignore_case opt 
                     then any (ciEqual a) 
                     else elem a 



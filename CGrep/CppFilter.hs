module CGrep.CppFilter (cgrepCppFilter) where

import Data.ByteString.Lazy.Search as LC

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import CGrep.Function
import CGrep.Output
import CGrep.Options 

import qualified CGrep.Cpp.Filter as Cpp

cgrepCppFilter :: CgrepFunction
cgrepCppFilter opt ps f = do
    source <- LC.readFile f
    let filtered =  Cpp.filter Cpp.ContextFilter { Cpp.getCode = code opt, Cpp.getComment = comment opt, Cpp.getLiteral = string opt } source
    let content = zip [1..] $ LC.lines filtered
    return $ concat $ map (if (word opt) then simpleLineGrep opt f ps
                                         else simpleWordGrep opt f lps) content
        where lps = map LC.fromChunks (map (:[]) ps)



simpleLineGrep :: Options -> FilePath -> [C.ByteString] -> (Int, LC.ByteString) -> [Output]
simpleLineGrep opt f ps (n, l) = 
   if ((null tokens) `xor` (invert_match opt)) 
     then []
     else [LazyOutput f n l (map C.unpack tokens)]
   where tokens  = filter (\p -> not . null $ LC.indices p l) ps   



simpleWordGrep :: Options -> FilePath -> [LC.ByteString] -> (Int, LC.ByteString) -> [Output]
simpleWordGrep opt f ps (n, l) = 
   if ((null tokens) `xor` (invert_match opt)) 
     then []
     else [LazyOutput f n l (map LC.unpack tokens)]
   where tokens  = filter (`elem` (LC.words l)) ps   



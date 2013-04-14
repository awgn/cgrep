module CGrep.CppFilter (cgrepCppFilter) where

import Data.ByteString.Lazy.Search as LC

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import CGrep.Function
import CGrep.Output
import CGrep.Options 

import qualified CGrep.Cpp.Filter as Cpp

cgrepCppFilter :: CgrepFunction
cgrepCppFilter opt pats f = do
    source <- LC.readFile f
    let filtered =  Cpp.filter Cpp.ContextFilter { Cpp.getCode = code opt, Cpp.getComment = comment opt, Cpp.getLiteral = string opt } source
    let content = (zip [1..]) $ LC.lines filtered
    return $ concat $ map (simpleWordsGrep opt f pats) content


xor :: Bool -> Bool -> Bool
a `xor` b = a && (not b) || (not a) && b


simpleWordsGrep :: Options -> FilePath -> [C.ByteString] -> (Int, LC.ByteString) -> [Output]
simpleWordsGrep opt f ps (n, l) =
    if ((null pfilt) `xor` (invert_match opt)) 
      then []
      else [LazyOutput f n l pfilt]
    where pfilt = filter (\p -> not . null $ LC.indices p l) ps    

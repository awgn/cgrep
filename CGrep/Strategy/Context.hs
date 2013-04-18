module CGrep.Strategy.Context (cgrepCppContext) where

import Data.ByteString.Lazy.Search as LC

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import CGrep.Function
import CGrep.Output
import CGrep.Options 

import CGrep.StringLike

import Control.Monad (when)

import qualified CGrep.Cpp.Filter as Cpp

cgrepCppContext :: CgrepFunction
cgrepCppContext opt ps f = do
    source <- lazyReadFile f
    
    let filtered =  Cpp.filter Cpp.ContextFilter { Cpp.getCode = code opt, Cpp.getComment = comment opt, Cpp.getLiteral = literal opt } source
    let content = zip [1..] $ map toStrict (LC.lines filtered) :: [(Int,C.ByteString)]

    when (debug opt) $ print content

    return $ concatMap (basicGrep opt f ps) content



module CGrep.Function where

import CGrep.Options
import CGrep.Output

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

type CgrepFunction = Options -> [C.ByteString] -> FilePath -> IO [Output] 


xor :: Bool -> Bool -> Bool
a `xor` b = a && not b || not a && b


strictReadFile :: FilePath -> IO C.ByteString
strictReadFile "" = C.getContents
strictReadFile f  = C.readFile f


lazyReadFile :: FilePath -> IO LC.ByteString
lazyReadFile "" = LC.getContents
lazyReadFile f  = LC.readFile f

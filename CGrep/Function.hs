module CGrep.Function where

import CGrep.Options
import CGrep.Output

import qualified Data.ByteString.Char8 as C

type CgrepFunction = Options -> [C.ByteString] -> FilePath -> IO [Output] 

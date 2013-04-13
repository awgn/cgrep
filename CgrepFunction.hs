module CgrepFunction where

import Options
import Output

import qualified Data.ByteString.Char8 as C

type CgrepFunction = Options -> [C.ByteString] -> FilePath -> IO [Output] 

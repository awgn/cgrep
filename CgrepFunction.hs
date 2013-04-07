module CgrepFunction where

import Options
import Output

import qualified Data.ByteString.Char8 as B

type CgrepFunction = Options -> [B.ByteString] -> FilePath -> IO [Output] 

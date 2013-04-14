module CGrep.Function where

import CGrep.Options
import CGrep.Output

import qualified Data.ByteString.Char8 as C

type CgrepFunction = Options -> [C.ByteString] -> FilePath -> IO [Output] 


xor :: Bool -> Bool -> Bool
a `xor` b = a && (not b) || (not a) && b




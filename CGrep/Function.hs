module CGrep.Function where

import CGrep.Options
import CGrep.Output
import CGrep.StringLike

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

type CgrepFunction = Options -> [C.ByteString] -> FilePath -> IO [Output] 


basicGrep :: (StringLike a) => Options -> FilePath -> [a] -> (Int, a) -> [Output]
basicGrep opt f patterns (n, line) =
    if null patfilt
      then []
      else [Output f n line (map slToString patfilt)]
    where patfilt = slGrep (word opt) (invert_match opt) patterns line  


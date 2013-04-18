module CGrep.Function where

import CGrep.Options
import CGrep.Output
import CGrep.StringLike

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

type CgrepFunction = Options -> [C.ByteString] -> FilePath -> IO [Output] 


strictReadFile :: FilePath -> IO C.ByteString
strictReadFile "" = C.getContents
strictReadFile f  = C.readFile f


lazyReadFile :: FilePath -> IO LC.ByteString
lazyReadFile "" = LC.getContents
lazyReadFile f  = LC.readFile f


basicGrep :: (StringLike a) => Options -> FilePath -> [a] -> (Int, a) -> [Output]
basicGrep opt f patterns (n, line) =
    if null patfilt
      then []
      else [Output f n line (map toString patfilt)]
    where patfilt = grep (word opt) (ignore_case opt) (invert_match opt) patterns line  


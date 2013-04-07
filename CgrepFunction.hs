module CgrepFunction where

import Options
import Output

import qualified Data.Text as T
import qualified Data.Text.IO as T

type CgrepFunction = Options -> [T.Text] -> FilePath -> IO [Output] 

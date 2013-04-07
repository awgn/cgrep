module CgrepFunction where

import Options
import Output

type CgrepFunction = Options -> [String] -> FilePath -> IO [Output] 

module CgrepFunction where

import Options

type CgrepFunction = Options -> [String] -> FilePath -> IO [(FilePath, Int, String)] 

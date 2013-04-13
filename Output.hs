module Output where

import qualified Data.ByteString.Char8 as C
import Options

data Output = Output FilePath Int C.ByteString [C.ByteString]
                deriving (Show,Eq)


showOutput :: Options -> Output -> String
showOutput Options { no_filename = False, no_linenumber = False } (Output f n l _) = f ++ ":" ++ show n ++ ":" ++ show l
showOutput Options { no_filename = False, no_linenumber = True  } (Output f _ l _) = f ++ ":" ++ show l
showOutput Options { no_filename = True , no_linenumber = False } (Output _ n l _) = show n ++ ":" ++ show l
showOutput Options { no_filename = True , no_linenumber = True  } (Output _ _ l _) = show l

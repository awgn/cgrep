module CGrep.Output where

import qualified Data.ByteString.Char8 as C
import CGrep.Options

data Output = Output FilePath Int C.ByteString [C.ByteString]
                deriving (Show,Eq)


showOutput :: Options -> Output -> String
showOutput Options { no_filename = False, no_linenumber = False } (Output f n l _) = f ++ ":" ++ show n ++ ":" ++ C.unpack l
showOutput Options { no_filename = False, no_linenumber = True  } (Output f _ l _) = f ++ ":" ++ C.unpack l
showOutput Options { no_filename = True , no_linenumber = False } (Output _ n l _) = show n ++ ":" ++ C.unpack l
showOutput Options { no_filename = True , no_linenumber = True  } (Output _ _ l _) = C.unpack l

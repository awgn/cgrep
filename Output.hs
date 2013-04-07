module Output where

import qualified Data.ByteString.Char8 as B
import Options

data Output = Output FilePath Int B.ByteString [B.ByteString]
                deriving (Show,Eq)


showOutput :: Options -> Output -> String
showOutput Options { no_filename = False, no_linenumber = False } (Output f n l _) = f ++ ":" ++ show n ++ ":" ++ (B.unpack l)
showOutput Options { no_filename = False, no_linenumber = True  } (Output f n l _) = f ++ ":" ++ (B.unpack l)
showOutput Options { no_filename = True , no_linenumber = False } (Output f n l _) = show n ++ ":" ++ (B.unpack l)
showOutput Options { no_filename = True , no_linenumber = True  } (Output f n l _) = (B.unpack l)

module Output where

import qualified Data.ByteString.Char8 as B
import Options

data Output = Output FilePath Int B.ByteString [B.ByteString]
                deriving (Show,Eq)


showOutput :: Options -> Output -> String
showOutput _ (Output f n l _) = f ++ ":" ++ show n ++ ":" ++ (B.unpack l)

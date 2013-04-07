module Output where

import qualified Data.Text as T
import Options

data Output = Output FilePath Int T.Text [String]
                deriving (Show,Eq)


showOutput :: Options -> Output -> String
showOutput _ (Output f n l _) = f ++ ":" ++ show n ++ ":" ++ (T.unpack l)

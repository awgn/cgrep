module CGrep.SimpleWords (cgrepSimpleWords) where

import qualified Data.ByteString.Char8 as C
import Control.Monad(liftM)

import CGrep.Function
import CGrep.Output
import CGrep.Options 


cgrepSimpleWords :: CgrepFunction
cgrepSimpleWords opt pats f = do
    content <- liftM (zip [1..]) $ liftM C.lines (C.readFile f)
    return $ concat $ map (simpleWordsGrep opt f pats) content


xor :: Bool -> Bool -> Bool
a `xor` b = a && (not b) || (not a) && b


simpleWordsGrep :: Options -> FilePath -> [C.ByteString] -> (Int, C.ByteString) -> [Output]
simpleWordsGrep opt f ps (n, l) =
    if ((null pfilt) `xor` (invert_match opt)) 
      then []
      else [StrictOutput f n l pfilt]
    where pfilt = filter (`elem` C.words l) ps    

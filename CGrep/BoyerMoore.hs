module CGrep.BoyerMoore (cgrepBoyerMoore) where

import Data.ByteString.Search as C

import qualified Data.ByteString.Char8 as C
import Control.Monad(liftM)

import CGrep.Function
import CGrep.Output
import CGrep.Options 


cgrepBoyerMoore :: CgrepFunction
cgrepBoyerMoore opt pats f = do
    content <- liftM (zip [1..]) $ liftM C.lines (C.readFile f)
    return $ concat $ map (simpleBoyerMoore opt f pats) content


xor :: Bool -> Bool -> Bool
a `xor` b = a && (not b) || (not a) && b


simpleBoyerMoore :: Options -> FilePath -> [C.ByteString] -> (Int, C.ByteString) -> [Output]
simpleBoyerMoore opt f ps (n, l) =
    if ((null pfilt) `xor` (invert_match opt)) then []
                    else [Output f n l pfilt]
    where pfilt = filter (\p -> not . null $ C.indices p l) ps    

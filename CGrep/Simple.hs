module CGrep.Simple (cgrepSimpleLine, cgrepSimpleToken) where

import qualified Data.ByteString.Char8 as C
import Control.Monad(liftM)

import CGrep.Function
import CGrep.Output
import CGrep.Options 


cgrepSimpleLine :: CgrepFunction
cgrepSimpleLine opt pats f = do
    content <- liftM (zip [1..]) $ liftM C.lines (C.readFile f)
    return $ concat $ map (simpleLineGrep opt f pats) content


cgrepSimpleToken :: CgrepFunction
cgrepSimpleToken opt pats f = do
    content <- liftM (zip [1..]) $ liftM C.lines (C.readFile f)
    return $ concat $ map (simpleTokenGrep opt f pats) content


---------------------------------------------------------------

xor :: Bool -> Bool -> Bool
a `xor` b = a && (not b) || (not a) && b


simpleLineGrep :: Options -> FilePath -> [C.ByteString] -> (Int, C.ByteString) -> [Output]
simpleLineGrep opt f ps (n, l) =
    if ((null pfilt) `xor` (invert_match opt)) then []
                    else [Output f n l pfilt]
    where pfilt = filter (`C.isInfixOf` l) ps    


simpleTokenGrep :: Options -> FilePath -> [C.ByteString] -> (Int, C.ByteString) -> [Output]
simpleTokenGrep opt f ps (n, l) =
    if ((null pfilt) `xor` (invert_match opt)) then []
                    else [Output f n l pfilt]
    where pfilt = filter (`elem` C.words l) ps    

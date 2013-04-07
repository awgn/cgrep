module CgrepSimple (cgrepSimpleLine, cgrepSimpleToken) where

import CgrepFunction
import Output
import Options 

import qualified Data.ByteString.Char8 as B
import Control.Monad(liftM)

cgrepSimpleLine :: CgrepFunction
cgrepSimpleLine opt pats f = do
    content <- liftM (zip [1..]) $ liftM B.lines (B.readFile f)
    return $ concat $ map (simpleLineGrep opt f pats) content


cgrepSimpleToken :: CgrepFunction
cgrepSimpleToken opt pats f = do
    content <- liftM (zip [1..]) $ liftM B.lines (B.readFile f)
    return $ concat $ map (simpleTokenGrep opt f pats) content


---------------------------------------------------------------

xor :: Bool -> Bool -> Bool
a `xor` b = a && (not b) || (not a) && b


simpleLineGrep :: Options -> FilePath -> [B.ByteString] -> (Int, B.ByteString) -> [Output]
simpleLineGrep opt f ps (n, l) =
    if ((null pfilt) `xor` (invert_match opt)) then []
                    else [Output f n l pfilt]
    where pfilt = filter (`B.isInfixOf` l) ps    


simpleTokenGrep :: Options -> FilePath -> [B.ByteString] -> (Int, B.ByteString) -> [Output]
simpleTokenGrep opt f ps (n, l) =
    if ((null pfilt) `xor` (invert_match opt)) then []
                    else [Output f n l pfilt]
    where pfilt = filter (`elem` B.words l) ps    

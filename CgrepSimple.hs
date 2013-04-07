module CgrepSimple (cgrepSimpleLine, cgrepSimpleToken) where

import CgrepFunction
import Output

import qualified Data.ByteString.Char8 as B
import Control.Monad(liftM)

cgrepSimpleLine :: CgrepFunction
cgrepSimpleLine opt pats f = do
    content <- liftM (zip [1..]) $ liftM B.lines (B.readFile f)
    return $ concat $ map (simpleLineGrep f pats) content


cgrepSimpleToken :: CgrepFunction
cgrepSimpleToken opt pats f = do
    content <- liftM (zip [1..]) $ liftM B.lines (B.readFile f)
    return $ concat $ map (simpleTokenGrep f pats) content


---------------------------------------------------------------


simpleLineGrep :: FilePath -> [B.ByteString] -> (Int, B.ByteString) -> [Output]
simpleLineGrep f ps (n, l) =
    if (null pfilt) then []
                    else [Output f n l pfilt]
    where pfilt = filter (`B.isInfixOf` l) ps    


simpleTokenGrep :: FilePath -> [B.ByteString] -> (Int, B.ByteString) -> [Output]
simpleTokenGrep f ps (n, l) =
    if (null pfilt) then []
                    else [Output f n l pfilt]
    where pfilt = filter (`elem` B.words l) ps    

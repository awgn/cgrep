module CgrepSimple (cgrepSimpleLine, cgrepSimpleToken) where

import CgrepFunction
import Output

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad(liftM)

cgrepSimpleLine :: CgrepFunction
cgrepSimpleLine opt pats f = do
    content <- liftM (zip [1..]) $ liftM T.lines (T.readFile f)
    return $ concat $ map (simpleLineGrep f pats) content


cgrepSimpleToken :: CgrepFunction
cgrepSimpleToken opt pats f = do
    content <- liftM (zip [1..]) $ liftM T.lines (T.readFile f)
    return $ concat $ map (simpleTokenGrep f pats) content


---------------------------------------------------------------


simpleLineGrep :: FilePath -> [T.Text] -> (Int, T.Text) -> [Output]
simpleLineGrep f ps (n, l) =
    if (null pfilt) then []
                    else [Output f n l pfilt]
    where pfilt = filter (`T.isInfixOf` l) ps    


simpleTokenGrep :: FilePath -> [T.Text] -> (Int, T.Text) -> [Output]
simpleTokenGrep f ps (n, l) =
    if (null pfilt) then []
                    else [Output f n l pfilt]
    where pfilt = filter (`elem` T.words l) ps    

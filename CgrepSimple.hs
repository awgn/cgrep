module CgrepSimple where

import CgrepFunction

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad(liftM)


cgrepSimpleLine :: CgrepFunction
cgrepSimpleLine opt pats f = do
    content <- liftM T.lines (T.readFile f)
    let filt = filter (T.isInfixOf (T.pack $ head pats)) content
    return $ map (\l -> (f, 0, T.unpack l)) filt



cgrepSimpleToken :: CgrepFunction 
cgrepSimpleToken opt pats f = do
    content <- liftM T.lines (T.readFile f)
    let filt = filter (T.isInfixOf (T.pack $ head pats)) content
    return $ map (\l -> (f, 0, T.unpack l)) filt

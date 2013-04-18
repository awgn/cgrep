module CGrep.Strategy.Simple (cgrepSimple) where

import qualified Data.ByteString.Char8 as C

import Control.Monad(liftM,when)

import CGrep.Function
import CGrep.Output
import CGrep.Options 

import CGrep.StringLike

cgrepSimple :: CgrepFunction
cgrepSimple opt ps f = do
    content <- liftM (zip [1..] . C.lines) (strictReadFile f)

    when (debug opt) $ print content
    
    return $ concatMap (basicGrep opt f ps) content


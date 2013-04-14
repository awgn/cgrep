module CGrep.Simple (cgrepSimple) where

import qualified Data.ByteString.Char8 as C

import Control.Monad(liftM)

import CGrep.Function
import CGrep.Output
import CGrep.Options 


cgrepSimple :: CgrepFunction
cgrepSimple opt ps f = do
    content <- liftM (zip [1..]) $ liftM C.lines (C.readFile f)
    return $ concat $ map (if (word opt) then simpleWordGrep opt f ps
                                         else simpleLineGrep opt f ps) content


simpleLineGrep :: Options -> FilePath -> [C.ByteString] -> (Int, C.ByteString) -> [Output]
simpleLineGrep opt f ps (n, l) =
    if ((null pfilt) `xor` (invert_match opt)) 
      then []
      else [StrictOutput f n l (map C.unpack pfilt)]
    where pfilt = filter (`C.isInfixOf` l) ps    


simpleWordGrep :: Options -> FilePath -> [C.ByteString] -> (Int, C.ByteString) -> [Output]
simpleWordGrep opt f ps (n, l) =
    if ((null pfilt) `xor` (invert_match opt)) 
      then []
      else [StrictOutput f n l (map C.unpack pfilt)]
    where pfilt = filter (`elem` C.words l) ps    



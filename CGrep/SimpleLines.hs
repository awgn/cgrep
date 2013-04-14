module CGrep.SimpleLines (cgrepSimpleLines) where

import qualified Data.ByteString.Char8 as C
import Control.Monad(liftM)

import CGrep.Function
import CGrep.Output
import CGrep.Options 


cgrepSimpleLines :: CgrepFunction
cgrepSimpleLines opt pats f = do
    content <- liftM (zip [1..]) $ liftM C.lines (C.readFile f)
    return $ concat $ map (simpleLineGrep opt f pats) content


xor :: Bool -> Bool -> Bool
a `xor` b = a && (not b) || (not a) && b


simpleLineGrep :: Options -> FilePath -> [C.ByteString] -> (Int, C.ByteString) -> [Output]
simpleLineGrep opt f ps (n, l) =
    if ((null pfilt) `xor` (invert_match opt)) 
      then []
      else [StrictOutput f n l (map C.unpack pfilt)]
    where pfilt = filter (`C.isInfixOf` l) ps    

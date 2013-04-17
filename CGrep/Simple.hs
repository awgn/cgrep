module CGrep.Simple (cgrepSimple) where

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
    
    return $ concatMap (if word opt then simpleWordGrep opt f ps
                                    else simpleLineGrep opt f ps) content


simpleLineGrep :: Options -> FilePath -> [C.ByteString] -> (Int, C.ByteString) -> [Output]
simpleLineGrep opt f ps (n, l) =
    if null pfilt `xor` invert_match opt 
      then []
      else [StrictOutput f n l (map C.unpack pfilt)]
    where pfilt = filter (`subMatch` l) ps    
          subMatch = if ignore_case opt 
                       then ciIsInfixOf 
                       else C.isInfixOf


simpleWordGrep :: Options -> FilePath -> [C.ByteString] -> (Int, C.ByteString) -> [Output]
simpleWordGrep opt f ps (n, l) =
    if null pfilt `xor` invert_match opt 
      then []
      else [StrictOutput f n l (map C.unpack pfilt)]
    where pfilt = filter (`match` gwords l) ps    
          match a = if ignore_case opt 
                      then any (ciEqual a) 
                      else elem a 
            


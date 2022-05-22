module CGrep.Boundary ( Boundary(..)) where

import qualified Data.ByteString.Char8 as C

data Boundary = Boundary {
    bBegin :: C.ByteString
 ,  bEnd :: C.ByteString
} deriving (Show, Eq)

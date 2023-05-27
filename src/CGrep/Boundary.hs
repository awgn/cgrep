{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnliftedNewtypes #-}

module CGrep.Boundary (
     Boundary(..)
    , BoundaryType(..)
    , pattern Begin
    , pattern End) where

import qualified Data.ByteString.Char8 as C

data Boundary = Boundary {
    bBegin :: C.ByteString
 ,  bEnd :: C.ByteString
} deriving (Show, Eq)


data BoundaryType = BoundaryType (# () | () #)

instance Show BoundaryType where
    show Begin = "begin"
    show End = "end"


pattern Begin :: BoundaryType
pattern Begin = BoundaryType (# () | #)
pattern End :: BoundaryType
pattern End = BoundaryType (# | () #)

{-# COMPLETE Begin, End #-}
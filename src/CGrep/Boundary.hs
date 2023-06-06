{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module CGrep.Boundary (
     Boundary(..)
    , BoundaryType(..)
    , pattern Begin
    , pattern End) where

import qualified Data.ByteString.Char8 as C
import Data.Word

data Boundary = Boundary {
    bBegin :: C.ByteString
 ,  bEnd :: C.ByteString
} deriving (Show, Eq)


newtype BoundaryType = BoundaryType { unpackBoundaryType :: Word8}
    deriving newtype (Eq, Ord)

instance Show BoundaryType where
    show Begin = "begin"
    show End = "end"

pattern Begin :: BoundaryType
pattern Begin = BoundaryType 0
pattern End :: BoundaryType
pattern End = BoundaryType 1

{-# COMPLETE Begin, End #-}
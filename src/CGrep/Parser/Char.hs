module CGrep.Parser.Char where

import Data.Char ( isAlpha, isAlphaNum, isHexDigit )

isCharNumber :: Char -> Bool
isCharNumber c = isHexDigit c || c `elem` (".xX" :: String)

isAlpha_ :: Char -> Bool
isAlpha_ c = isAlpha c || c == '_'
{-# INLINE isAlpha_ #-}

isAlphaNum_ :: Char -> Bool
isAlphaNum_ c = isAlphaNum c || c == '_'
{-# INLINE isAlphaNum_ #-}

isAlpha_' :: Char -> Bool
isAlpha_' c = isAlpha c || c == '_' || c == '\''
{-# INLINE isAlpha_' #-}

isAlphaNum_' :: Char -> Bool
isAlphaNum_' c = isAlphaNum c || c == '_' || c == '\''
{-# INLINE isAlphaNum_' #-}

isBracket' :: Char -> Bool
isBracket' c = c `elem` ("[]{}()" :: String)
{-# INLINE isBracket' #-}
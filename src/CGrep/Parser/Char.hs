module CGrep.Parser.Char where
import Data.Char ( isAlphaNum, isAlpha, isHexDigit )


isCharNumberC :: Char -> Bool
isCharNumberC c = isHexDigit c || c `elem` (".xX" :: String)

isAlphaC :: Char -> Bool
isAlphaC c = isAlpha c || c == '_'
{-# INLINE isAlphaC  #-}


isAlphaNumC :: Char -> Bool
isAlphaNumC c = isAlphaNum c || c == '_' || c == '\''
{-# INLINE isAlphaNumC #-}


isBracketC :: Char -> Bool
isBracketC = (`elem` ("{[()]}" :: String))
{-# INLINE isBracketC #-}
{-# LANGUAGE MagicHash #-}


module CGrep.Parser.Char where

import GHC.Exts ( Char(C#), Int(I#), ord# )
import GHC.Base (isTrue#, int2Word#, leWord#, chr#)

ord :: Char -> Int
ord (C# c#) = I# (ord# c#)
{-# INLINE ord #-}


chr :: Int -> Char
chr i@(I# i#)
 | isTrue# (int2Word# i# `leWord#` 0x10FFFF##) = C# (chr# i#)
 | otherwise
    = errorWithoutStackTrace ("CGrep.Char: bad argument: " ++ show i)
{-# INLINE chr #-}


isDigit :: Char -> Bool
isDigit c =  (fromIntegral (ord c - ord '0') :: Word) <= 9
{-# INLINE isDigit #-}


isSpace :: Char -> Bool
isSpace c = uc == 32 || uc - 0x9 <= 4 || uc == 0xa0
  where
    uc = ord c
{-# INLINE isSpace #-}


isHexDigit   :: Char -> Bool
isHexDigit c =  isDigit c ||
                (fromIntegral (ord c - ord 'A')::Word) <= 5 ||
                (fromIntegral (ord c - ord 'a')::Word) <= 5
{-# INLINE isHexDigit #-}


isCharNumber :: Char -> Bool
isCharNumber c = isHexDigit c || c `elem` (".xX" :: String)
{-# INLINE isCharNumber #-}


isAlphaNum :: Char -> Bool
isAlphaNum c = o >= 97 && o <= 122  ||
                o >= 65 && o <= 90  ||
                o >= 48 && o <= 57
    where o = ord c
{-# INLINE isAlphaNum #-}


isAlpha :: Char -> Bool
isAlpha c = o >= 97 && o <= 122 ||
            o >= 65 && o <= 90
    where o = ord c
{-# INLINE isAlpha #-}


isAlphaNum_ :: Char -> Bool
isAlphaNum_ c = o >= 97 && o <= 122 ||
                o >= 65 && o <= 90  ||
                o >= 48 && o <= 57  || c == '_'
    where o = ord c
{-# INLINE isAlphaNum_ #-}


isAlpha_ :: Char -> Bool
isAlpha_ c = o >= 97 && o <= 122 ||
             o >= 65 && o <= 90  || c == '_'
    where o = ord c
{-# INLINE isAlpha_ #-}


isAlpha_' :: Char -> Bool
isAlpha_' c = isAlpha_ c || c == '_' || c == '\''
{-# INLINE isAlpha_' #-}


isAlphaNum_' :: Char -> Bool
isAlphaNum_' c = isAlphaNum_ c || c == '_' || c == '\''
{-# INLINE isAlphaNum_' #-}


isBracket' :: Char -> Bool
isBracket' c = c `elem` ("[]{}()" :: String)
{-# INLINE isBracket' #-}


isAlpha_and :: String -> Char -> Bool
isAlpha_and s c = isAlpha_ c || c == '_' || c `elem` s
{-# INLINE isAlpha_and #-}


isAlphaNum_and :: String -> Char -> Bool
isAlphaNum_and s c = isAlphaNum_ c || c == '_' || c `elem` s
{-# INLINE isAlphaNum_and #-}

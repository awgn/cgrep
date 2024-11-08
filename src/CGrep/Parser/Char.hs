--
-- Copyright (c) 2013-2023 Nicola Bonelli <nicola@larthia.com>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--

module CGrep.Parser.Char (
    chr,
    ord,
    isDigit,
    isSpace,
    isHexDigit,
    isCharNumber,
    isAlphaNum,
    isAlpha,
    isAlphaNum_,
    isAlpha_,
    isAlpha_',
    isAlphaNum_',
    isBracket',
    isPunctuation,
    isAlpha_and,
    isAlphaNum_and,
) where

import GHC.Base (chr#, int2Word#, isTrue#, leWord#)
import GHC.Exts (Char (C#), Int (I#), ord#)

ord :: Char -> Int
ord (C# c#) = I# (ord# c#)
{-# INLINE ord #-}

chr :: Int -> Char
chr i@(I# i#)
    | isTrue# (int2Word# i# `leWord#` 0x10FFFF##) = C# (chr# i#)
    | otherwise =
        errorWithoutStackTrace ("CGrep: chr bad argument: " <> show i)
{-# INLINE chr #-}

isDigit :: Char -> Bool
isDigit c = (fromIntegral (ord c - ord '0') :: Word) <= 9
{-# INLINE isDigit #-}

isSpace :: Char -> Bool
isSpace c = uc == 32 || uc == 0xa0 || (uc - 0x9 <= 4) && not ctrl
  where
    uc = ord c
    ctrl = uc == 2 || uc == 3
{-# INLINE isSpace #-}

isHexDigit :: Char -> Bool
isHexDigit c =
    isDigit c
        || (fromIntegral (ord c - ord 'A') :: Word) <= 5
        || (fromIntegral (ord c - ord 'a') :: Word) <= 5
{-# INLINE isHexDigit #-}

isCharNumber :: Char -> Bool
isCharNumber c = isHexDigit c || c `elem` (".xX" :: String)
{-# INLINE isCharNumber #-}

isAlphaNum :: Char -> Bool
isAlphaNum c =
    o >= 97 && o <= 122
        || o >= 65 && o <= 90
        || o >= 48 && o <= 57
  where
    o = ord c
{-# INLINE isAlphaNum #-}

isAlpha :: Char -> Bool
isAlpha c =
    o >= 97 && o <= 122
        || o >= 65 && o <= 90
  where
    o = ord c
{-# INLINE isAlpha #-}

isAlphaNum_ :: Char -> Bool
isAlphaNum_ c =
    o >= 97 && o <= 122
        || o >= 65 && o <= 90
        || o >= 48 && o <= 57
        || c == '_'
  where
    o = ord c
{-# INLINE isAlphaNum_ #-}

isAlpha_ :: Char -> Bool
isAlpha_ c =
    o >= 97 && o <= 122
        || o >= 65 && o <= 90
        || c == '_'
  where
    o = ord c
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

isPunctuation :: Char -> Bool
isPunctuation c = c `elem` (":;,." :: String)
{-# INLINE isPunctuation #-}

isAlpha_and :: String -> Char -> Bool
isAlpha_and s c = isAlpha_ c || c == '_' || c `elem` s
{-# INLINE isAlpha_and #-}

isAlphaNum_and :: String -> Char -> Bool
isAlphaNum_and s c = isAlphaNum_ c || c == '_' || c `elem` s
{-# INLINE isAlphaNum_and #-}

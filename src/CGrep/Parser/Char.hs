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
{-# LANGUAGE OverloadedStrings #-}

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
    isAlphaDollar_,
    isAlphaNumDollar_,
    isUnicode_,
    isUnicodeNum_,
    isUnicodeDollar_,
    isUnicodeNumDollar_,
    isUnicodeNum_',
    isUnicodeXIDStart_,
    isUnicodeNumXIDCont_,
    isBracket',
    isPunctuation,
    isClojureIdentStart,
    isClojureIdentCont,
    isAlphaDash_,
    isAlphaNumDash_,
    isCSharpIdentStart,
    isCSharpIdentCont,
    isHtmlIdentStart,
    isHtmlIdentCont,
    isJavaIdentStart,
    isJavaIdentCont,
    isJuliaIdentStart,
    isJuliaIdentCont,
    isLispIdent,
    isAgdaIdent,
) where

import Data.Char (GeneralCategory (..), generalCategory, isLetter)
import GHC.Base (chr#, int2Word#, isTrue#, leWord#)
import GHC.Exts (Char (C#), Int (I#), ord#)
import GHC.Unicode (isAsciiLower)

ord :: Char -> Int
ord (C# c#) = I# (ord# c#)
{-# INLINE ord #-}

chr :: Int -> Char
chr i@(I# i#)
    | isTrue# (int2Word# i# `leWord#` 0x10FFFF##) = C# (chr# i#)
    | otherwise =
        errorWithoutStackTrace ("CGrep: chr bad argument: " <> show i)
{-# INLINE chr #-}

isBracket' :: Char -> Bool
isBracket' c = c `elem` ("[]{}()" :: String)
{-# INLINE isBracket' #-}

isPunctuation :: Char -> Bool
isPunctuation c = c `elem` (";,." :: String)
{-# INLINE isPunctuation #-}

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

isAlphaNumDash_ :: Char -> Bool
isAlphaNumDash_ c =
    o >= 97 && o <= 122
        || o >= 65 && o <= 90
        || o >= 48 && o <= 57
        || c == '_'
        || c == '-'
  where
    o = ord c
{-# INLINE isAlphaNumDash_ #-}

isAlpha_ :: Char -> Bool
isAlpha_ c =
    o >= 97 && o <= 122
        || o >= 65 && o <= 90
        || c == '_'
  where
    o = ord c
{-# INLINE isAlpha_ #-}

isAlphaDash_ :: Char -> Bool
isAlphaDash_ c =
    o >= 97 && o <= 122
        || o >= 65 && o <= 90
        || c == '_'
        || c == '-'
  where
    o = ord c
{-# INLINE isAlphaDash_ #-}

isAlpha_' :: Char -> Bool
isAlpha_' c = isAlpha_ c || c == '_' || c == '\''
{-# INLINE isAlpha_' #-}

isAlphaNum_' :: Char -> Bool
isAlphaNum_' c = isAlphaNum_ c || c == '_' || c == '\''
{-# INLINE isAlphaNum_' #-}

isAlphaDollar_ :: Char -> Bool
isAlphaDollar_ c = isAlphaNum_ c || c == '$'
{-# INLINE isAlphaDollar_ #-}

isAlphaNumDollar_ :: Char -> Bool
isAlphaNumDollar_ c = isAlphaNum_ c || c == '_' || c == '$'
{-# INLINE isAlphaNumDollar_ #-}

isClojureIdentStart :: Char -> Bool
isClojureIdentStart c
    | isSpace c = False
    | isClojureDelimiter c = False
    | isDigit c = False
    | c `elem` ("#:" :: String) = False
    | otherwise = True
  where
    isClojureDelimiter x = x `elem` ("()[]{}@;~`'\"\\" :: String)
{-# INLINE isClojureIdentStart #-}

isClojureIdentCont :: Char -> Bool
isClojureIdentCont c = isClojureIdentStart c || isDigit c
{-# INLINE isClojureIdentCont #-}

isCSharpIdentStart :: Char -> Bool
isCSharpIdentStart c =
    case generalCategory c of
        UppercaseLetter -> True -- Lu
        LowercaseLetter -> True -- Ll
        TitlecaseLetter -> True -- Lt
        ModifierLetter -> True -- Lm
        OtherLetter -> True -- Lo
        LetterNumber -> True -- Nl
        _ -> c == '_' -- underscore permesso
{-# INLINE isCSharpIdentStart #-}

isCSharpIdentCont :: Char -> Bool
isCSharpIdentCont c =
    case generalCategory c of
        UppercaseLetter -> True -- Lu
        LowercaseLetter -> True -- Ll
        TitlecaseLetter -> True -- Lt
        ModifierLetter -> True -- Lm
        OtherLetter -> True -- Lo
        LetterNumber -> True -- Nl
        DecimalNumber -> True -- Nd
        ConnectorPunctuation -> True -- Pc (include _)
        NonSpacingMark -> True -- Mn
        SpacingCombiningMark -> True -- Mc
        Format -> True -- Cf
        _ -> False
{-# INLINE isCSharpIdentCont #-}

isHtmlIdentStart :: Char -> Bool
isHtmlIdentStart = isAlpha
{-# INLINE isHtmlIdentStart #-}

isHtmlIdentCont :: Char -> Bool
isHtmlIdentCont c = isAlphaNum c || c == '_' || c == '-' || c == '.' || c == ':'
{-# INLINE isHtmlIdentCont #-}

isJavaIdentStart :: Char -> Bool
isJavaIdentStart c = c == '_' || c == '$' || isLetter c
{-# INLINE isJavaIdentStart #-}

isJavaIdentCont :: Char -> Bool
isJavaIdentCont c = c == '_' || c == '$' || isLetter c || isDigit c
{-# INLINE isJavaIdentCont #-}

isJuliaIdentStart :: Char -> Bool
isJuliaIdentStart c = c == '_' || isLetter c
{-# INLINE isJuliaIdentStart #-}

isJuliaIdentCont :: Char -> Bool
isJuliaIdentCont c = c == '_' || c == '!' || c == '?' || isLetter c || isDigit c
{-# INLINE isJuliaIdentCont #-}

isUnicode_ :: Char -> Bool
isUnicode_ c = c == '_' || isLetter c
{-# INLINE isUnicode_ #-}

isUnicodeNum_ :: Char -> Bool
isUnicodeNum_ c = c == '_' || isLetter c || isDigit c
{-# INLINE isUnicodeNum_ #-}

isUnicodeDollar_ :: Char -> Bool
isUnicodeDollar_ c = c == '_' || c == '\'' || isLetter c || isDigit c
{-# INLINE isUnicodeDollar_ #-}

isUnicodeNumDollar_ :: Char -> Bool
isUnicodeNumDollar_ c = c == '_' || c == '$' || isLetter c || isDigit c
{-# INLINE isUnicodeNumDollar_ #-}

isUnicodeNum_' :: Char -> Bool
isUnicodeNum_' c = c == '_' || c == '\'' || isLetter c || isDigit c
{-# INLINE isUnicodeNum_' #-}

isLispIdent :: Char -> Bool
isLispIdent c =
    isAsciiLower c
        || isDigit c
        || c `elem` ("-+*/@$%^&_=<>~!?[]{}" :: String)

isUnicodeXIDStart_ :: Char -> Bool
isUnicodeXIDStart_ x = x == '_' || isXIDStart x
  where
    isXIDStart c = case generalCategory c of
        UppercaseLetter -> True
        LowercaseLetter -> True
        TitlecaseLetter -> True
        ModifierLetter -> True
        OtherLetter -> True
        LetterNumber -> True
        _ -> False
{-# INLINE isUnicodeXIDStart_ #-}

isUnicodeNumXIDCont_ :: Char -> Bool
isUnicodeNumXIDCont_ c = c == '_' || isXIDContinue c
  where
    isXIDContinue x = case generalCategory x of
        UppercaseLetter -> True
        LowercaseLetter -> True
        TitlecaseLetter -> True
        ModifierLetter -> True
        OtherLetter -> True
        LetterNumber -> True
        DecimalNumber -> True
        NonSpacingMark -> True
        SpacingCombiningMark -> True
        ConnectorPunctuation -> True
        _ -> False
{-# INLINE isUnicodeNumXIDCont_ #-}

isAgdaIdent :: Char -> Bool
isAgdaIdent c = not (c `elem` ("@.(){};_" :: String))
{-# INLINE isAgdaIdent #-}

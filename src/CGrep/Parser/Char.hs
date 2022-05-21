{-# LANGUAGE RecordWildCards #-}
module CGrep.Parser.Char where

import Data.Char ( isAlphaNum, isAlpha, isHexDigit )
import CGrep.LanguagesMap ( LanguageInfo(..) )


isCharNumber :: Char -> Bool
isCharNumber c = isHexDigit c || c `elem` (".xX" :: String)

isAlpha1 :: Maybe LanguageInfo -> Char -> Bool
isAlpha1 Nothing c = isAlpha c
isAlpha1 (Just LanguageInfo {..}) c = isAlpha c || c `elem` fst langAdditonalValidIdentifChars
{-# INLINE isAlpha1  #-}

isAlphaN :: Maybe LanguageInfo -> Char -> Bool
isAlphaN Nothing c = isAlphaNum c
isAlphaN (Just LanguageInfo {..}) c = isAlphaNum c || c `elem` snd langAdditonalValidIdentifChars
{-# INLINE isAlphaN  #-}


isBracket' :: Char -> Bool
isBracket' = (`elem` ("{[()]}" :: String))
{-# INLINE isBracket' #-}

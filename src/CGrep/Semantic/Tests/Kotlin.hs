{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Kotlin () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    tToken,
 )
import CGrep.Semantic.Tests.Common
instance LanguageTestFilter 'Kotlin where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideKotlin keepTests tokens

-- | Helper for Kotlin to avoid swallowing braces of subsequent functions after abstract methods
findOpeningBraceKotlin :: Bool -> Int -> [Token] -> Maybe ([Token], [Token])
findOpeningBraceKotlin _ 0 _  = Nothing
findOpeningBraceKotlin _ _ [] = Nothing
findOpeningBraceKotlin canSeeDecl n (t:ts)
    | isTokenBracket t && tToken t == "{" = Just ([t], ts)
    | isTokenKeyword t && (tToken t == "fun" || tToken t == "class" || tToken t == "interface" || tToken t == "val" || tToken t == "var") =
        if canSeeDecl
        then case findOpeningBraceKotlin False (n - 1) ts of
                 Nothing -> Nothing
                 Just (pre, post) -> Just (t : pre, post)
        else Nothing
    | otherwise =
        case findOpeningBraceKotlin canSeeDecl (n - 1) ts of
            Nothing -> Nothing
            Just (pre, post) -> Just (t : pre, post)

processOutsideKotlin :: Bool -> [Token] -> [Token]
processOutsideKotlin _ [] = [] -- End of stream
processOutsideKotlin keepTests tokens
    | let (attrTokens, isTest, restAfterAttrs) = collectJavaAnnotations tokens, not (null attrTokens)
    =
           if isTest
           then
               case findOpeningBraceKotlin True 100 restAfterAttrs of
                   Nothing ->
                       if keepTests
                       then processOutsideKotlin keepTests restAfterAttrs
                       else attrTokens ++ processOutsideKotlin keepTests restAfterAttrs
                   Just (sigTokens, restAfterBrace) ->
                       let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                       in if keepTests
                          then attrTokens ++ sigTokens ++ bodyTokens ++ processOutsideKotlin keepTests finalRest
                          else processOutsideKotlin keepTests finalRest
           else
               if keepTests
               then processOutsideKotlin keepTests restAfterAttrs
               else attrTokens ++ processOutsideKotlin keepTests restAfterAttrs

processOutsideKotlin keepTests (t1:t2:ts)
    | isTokenIdentifier t1 &&
      (tToken t1 == "test" || tToken t1 == "xtest" || tToken t1 == "describe" || tToken t1 == "xdescribe" || tToken t1 == "it" || tToken t1 == "xit" || tToken t1 == "context" || tToken t1 == "xcontext") &&
      isTokenBracket t2 && tToken t2 == "("
    =
        case findOpeningBraceBounded 100 ts of
            Nothing ->
                if keepTests then processOutsideKotlin keepTests (t2:ts) else t1 : processOutsideKotlin keepTests (t2:ts)
            Just (sigTokens, restAfterBrace) ->
                let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                in if keepTests
                   then t1 : t2 : sigTokens ++ bodyTokens ++ processOutsideKotlin keepTests finalRest
                   else processOutsideKotlin keepTests finalRest

processOutsideKotlin keepTests (t:ts) =
    if keepTests
    then processOutsideKotlin keepTests ts
    else t : processOutsideKotlin keepTests ts

-- ------------------------------------------------------------------

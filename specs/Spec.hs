module Main (main) where

import ContextFilterSpec (contextFilterTests)
import LanguageTestParsersSpec (languageTestParsersTests)
import ParserTokenAtomSpec (parserTokenAtomTests)
import PropertySpec (propertyTests)
import SemanticTestFilterSpec (semanticTestFilterTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "cgrep corner-case regression suite"
            [ contextFilterTests
            , parserTokenAtomTests
            , semanticTestFilterTests
            , languageTestParsersTests
            , propertyTests
            ]

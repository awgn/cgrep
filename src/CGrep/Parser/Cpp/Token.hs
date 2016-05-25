--
-- Copyright (c) 2012-2013 Bonelli Nicola <bonelli@antifork.org>
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

{-# LANGUAGE ViewPatterns #-}

module CGrep.Parser.Cpp.Token(Token(..), TokenFilter(..),
                       Offset, tokenizer, tokenFilter, tokenCompare,
                       isIdentifier, isKeyword, isDirective, isLiteralNumber,
                       isHeaderName, isString, isChar, isOperOrPunct
                       )  where

import Data.Char
import Data.Maybe
import Control.Monad
import Data.Array.Unboxed

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as C

import CGrep.Parser.Token

data TokenizerState = TokenizerState Source {-# UNPACK #-} !Offset !CppState
type Source         = C.ByteString
type Offset         = Int


data Token =
    TokenIdentifier  { toString :: !String, toOffset :: {-# UNPACK #-} !Int  } |
    TokenDirective   { toString :: !String, toOffset :: {-# UNPACK #-} !Int  } |
    TokenKeyword     { toString :: !String, toOffset :: {-# UNPACK #-} !Int  } |
    TokenNumber      { toString :: !String, toOffset :: {-# UNPACK #-} !Int  } |
    TokenHeaderName  { toString :: !String, toOffset :: {-# UNPACK #-} !Int  } |
    TokenString      { toString :: !String, toOffset :: {-# UNPACK #-} !Int  } |
    TokenChar        { toString :: !String, toOffset :: {-# UNPACK #-} !Int  } |
    TokenOperOrPunct { toString :: !String, toOffset :: {-# UNPACK #-} !Int  }
       deriving (Show, Eq, Ord)


data TokenFilter = TokenFilter
    {   filtIdentifier :: !Bool
    ,   filtDirective  :: !Bool
    ,   filtKeyword    :: !Bool
    ,   filtHeader     :: !Bool
    ,   filtString     :: !Bool
    ,   filtNumber     :: !Bool
    ,   filtChar       :: !Bool
    ,   filtOper       :: !Bool
    } deriving (Eq)


instance SemanticToken Token where
    tkIsIdentifier  = isIdentifier
    tkIsString      = isString
    tkIsChar        = isChar
    tkIsNumber      = isLiteralNumber
    tkIsKeyword     = isKeyword
    tkEquivalent    = tokenCompare
    tkToString      = toString
    tkToOffset      = toOffset
    tkToIdentif     = TokenIdentifier


-- Tokenize the source code in a list of Token
-- Precondition: the C++ source code must be well-formed
--

tokenizer :: Source -> [Token]
tokenizer xs = runGetToken (TokenizerState ys n Null)
            where (ys, n) = dropWhite xs


tokenFilter :: TokenFilter -> Token -> Bool
tokenFilter filt TokenIdentifier{}  = filtIdentifier filt
tokenFilter filt TokenDirective{}   = filtDirective  filt
tokenFilter filt TokenKeyword{}     = filtKeyword    filt
tokenFilter filt TokenHeaderName{}  = filtHeader     filt
tokenFilter filt TokenNumber{}      = filtNumber     filt
tokenFilter filt TokenString{}      = filtString     filt
tokenFilter filt TokenChar{}        = filtChar       filt
tokenFilter filt TokenOperOrPunct{} = filtOper       filt


tokenCompare :: Token -> Token -> Bool
tokenCompare TokenIdentifier { toString = l } TokenIdentifier { toString = r } = l == r
tokenCompare TokenDirective  { toString = l } TokenDirective  { toString = r } = l == r
tokenCompare TokenKeyword    { toString = l } TokenKeyword    { toString = r } = l == r
tokenCompare TokenNumber     { toString = l } TokenNumber     { toString = r } = l == r
tokenCompare TokenHeaderName { toString = l } TokenHeaderName { toString = r } = l == r
tokenCompare TokenString     { toString = l } TokenString     { toString = r } = l == r
tokenCompare TokenChar       { toString = l } TokenChar       { toString = r } = l == r
tokenCompare TokenOperOrPunct{ toString = l } TokenOperOrPunct{ toString = r } = l == r
tokenCompare _ _ = False


isIdentifier :: Token -> Bool
isIdentifier TokenIdentifier{}  = True
isIdentifier _ = False


isKeyword :: Token -> Bool
isKeyword TokenKeyword{}  = True
isKeyword _ = False


isDirective :: Token -> Bool
isDirective TokenDirective {}  = True
isDirective _ = False


isLiteralNumber :: Token -> Bool
isLiteralNumber TokenNumber {} = True
isLiteralNumber _ = False


isHeaderName :: Token -> Bool
isHeaderName TokenHeaderName {}  = True
isHeaderName _ = False


isString :: Token -> Bool
isString TokenString {} = True
isString _ = False


isChar :: Token -> Bool
isChar TokenChar{} = True
isChar _ = False


isOperOrPunct :: Token -> Bool
isOperOrPunct TokenOperOrPunct {}  = True
isOperOrPunct _ = False


isIdentifierChar' :: Char -> Bool
isIdentifierChar' = ((listArray ('\0', '\255') (map (\c -> isAlphaNum c || c == '_' || c == '$') ['\0'..'\255']) :: UArray Char Bool) !)  -- GNU allows $ in identifier


isChar' :: Char -> Bool
isChar'= ((listArray ('\0', '\255') (map (\c -> isSpace c || c == '\\') ['\0'..'\255']) :: UArray Char Bool) !)

-- Drop leading whitespace and count them
--

{-# INLINE dropWhite #-}

dropWhite :: Source -> (Source, Offset)
dropWhite xs = (xs', doff)
    where xs'  = C.dropWhile isChar' xs
          doff = fromIntegral $ C.length xs - C.length xs'


data CppState = Null | Hash | Include | Define | Undef | If | Ifdef | Ifndef | Elif | Else | Endif |
                Line | Error | Pragma
                deriving (Show, Eq)


directiveKeys :: HM.HashMap String CppState
directiveKeys = HM.fromList [ ("#",             Hash),
                              ("include",       Include),
                              ("include_next",  Include),
                              ("define",        Define),
                              ("undef",         Undef),
                              ("if",            If),
                              ("ifdef",         Ifdef),
                              ("ifndef",        Ifndef),
                              ("elif",          Elif),
                              ("else",          Else),
                              ("endif",         Null),
                              ("line",          Line),
                              ("error",         Error),
                              ("pragma",        Pragma) ]


nextCppState :: String -> CppState -> CppState
nextCppState str pps
    | Hash <- pps = fromMaybe Null (HM.lookup str directiveKeys)
    | otherwise   = if str == "#" then Hash else Null


runGetToken :: TokenizerState -> [Token]

runGetToken (TokenizerState (C.uncons  -> Nothing) _ _) = []
runGetToken tstate = token : runGetToken ns
    where (token, ns) = getToken tstate


getToken :: TokenizerState -> (Token, TokenizerState)

getToken (TokenizerState (C.uncons -> Nothing) _ _) = error "getToken: internal error"
getToken (TokenizerState xs off state) =
    let token = fromJust $
                    getTokenDirective xs state       `mplus`
                    getTokenHeaderName xs state      `mplus`
                    getTokenNumber xs state          `mplus`
                    getTokenString xs state          `mplus`
                    getTokenChar xs state            `mplus`
                    getTokenIdOrKeyword xs state     `mplus`
                    getTokenOpOrPunct xs state
        tstring  = toString token
        len      = fromIntegral $ length tstring
        (xs', w) = dropWhite $ C.drop (fromIntegral len) xs
    in
        (token { toOffset = off }, TokenizerState xs' (off + len + w) (nextCppState tstring state))


getTokenIdOrKeyword, getTokenNumber,
    getTokenHeaderName, getTokenString,
    getTokenChar, getTokenOpOrPunct,
    getTokenDirective :: Source -> CppState -> Maybe Token


getTokenDirective xs  state
    | state == Hash = Just (TokenDirective name 0)
    | otherwise = Nothing
    where name = C.unpack $ C.takeWhile isIdentifierChar' xs


getTokenHeaderName  (C.uncons -> Nothing) _ = error "getTokenHeaderName: internal error"
getTokenHeaderName  xs@(C.uncons -> Just (x,_)) state
    | state /= Include  = Nothing
    | x == '<'          = Just $ TokenHeaderName (getLiteral '<'  '>'  False xs)   0
    | x == '"'          = Just $ TokenHeaderName (getLiteral '"'  '"'  False xs)   0
    | otherwise         = Just $ TokenHeaderName (C.unpack $ C.takeWhile isIdentifierChar' xs) 0

getTokenHeaderName _ _ = undefined


getTokenNumber ys@(C.uncons -> Just (x,_)) _
    | x == '.' || isDigit x  = let ts = getNumber ys NumberNothing in
                                case ts of
                                    ""     -> Nothing
                                    "."    -> Nothing
                                    _      -> Just $ TokenNumber ts 0
    | otherwise = Nothing
getTokenNumber (C.uncons -> Nothing) _ = Nothing
getTokenNumber _ _ = undefined


validHexSet, validOctSet, validDecSet :: HS.HashSet Char

validHexSet   = HS.fromList "0123456789abcdefABCDEFxX"
validOctSet   = HS.fromList "01234567"
validDecSet   = HS.fromList "0123456789"

data NumberState = NumberNothing | NumberOHF | NumberDec | NumberOct | NumberHex | NumberMayBeFloat | NumberFloat | NumberExp
                    deriving (Show,Eq,Enum)

getNumber :: C.ByteString -> NumberState -> String
-- getNumber xs s | trace ("state = " ++ show s) False = undefined

getNumber (C.uncons -> Nothing) _ = ""
getNumber (C.uncons -> Just (x,xs)) state
    |  state == NumberNothing = case () of _
                                                | x == '0'  -> x : getNumber xs NumberOHF
                                                | x == '.'  -> x : getNumber xs NumberMayBeFloat
                                                | isDigit x -> x : getNumber xs NumberDec
                                                | otherwise -> ""
    |  state == NumberOHF = case () of _
                                                | x `HS.member` validHexSet -> x : getNumber xs NumberHex
                                                | x == '.'  -> x : getNumber xs NumberMayBeFloat
                                                | isDigit x -> x : getNumber xs NumberOct
                                                | otherwise -> ""

    |  state == NumberDec = case () of _
                                                | x `HS.member` validDecSet -> x : getNumber xs NumberDec
                                                | x == '.'  -> x : getNumber xs NumberMayBeFloat
                                                | x == 'e' || x == 'E'  -> x : getNumber xs NumberExp
                                                | otherwise -> ""

    |  state == NumberOct = case () of _
                                                | x `HS.member` validOctSet -> x : getNumber xs NumberOct
                                                | otherwise -> ""

    |  state == NumberHex = case () of _
                                                | x `HS.member` validHexSet -> x : getNumber xs NumberHex
                                                | otherwise -> ""

    |  state == NumberMayBeFloat = case () of _
                                                | x `HS.member` validDecSet   -> x : getNumber xs NumberFloat
                                                | otherwise                  -> ""

    |  state == NumberFloat = case () of _
                                                | x `HS.member` validDecSet -> x : getNumber xs NumberFloat
                                                | x == 'e' || x == 'E'       -> x : getNumber xs NumberExp
                                                | otherwise                  -> ""

    |  state == NumberExp = case () of _
                                                | x `HS.member` validDecSet   -> x : getNumber xs NumberExp
                                                | x == '+' || x == '-'       -> x : getNumber xs NumberExp
                                                | otherwise                  -> ""

getNumber  _ _ = undefined


getTokenString xs@(C.uncons -> Just (x,_)) _
    | x == '"' = Just $ TokenString (getLiteral '"'  '"'  False xs) 0
    | otherwise = Nothing
getTokenString (C.uncons -> Nothing) _ = Nothing
getTokenString _ _ = Nothing


getTokenChar xs@(C.uncons -> Just (x,_)) _
    | x == '\'' = Just $ TokenChar (getLiteral '\'' '\'' False xs) 0
    | otherwise = Nothing
getTokenChar (C.uncons -> Nothing) _ = Nothing
getTokenChar _ _ = Nothing


getTokenIdOrKeyword xs@(C.uncons -> Just (x,_)) _
    | not $ isIdentifierChar' x  = Nothing
    | name `HS.member` keywords = Just $ TokenKeyword name 0
    | otherwise                 = Just $ TokenIdentifier name 0
                                    where name = C.unpack $ C.takeWhile isIdentifierChar' xs
getTokenIdOrKeyword (C.uncons -> Nothing) _ = Nothing
getTokenIdOrKeyword _ _ = Nothing


getTokenOpOrPunct source _ = go source (min 4 (C.length source))
    where go _ 0
            | C.length source > 0 = error $ "operator or punct: error " ++ show source
            | otherwise = Nothing
          go src len
            | sub `HS.member` operOrPunct = Just $ TokenOperOrPunct sub 0
            | otherwise = go src (len-1)
                where sub = C.unpack (C.take len src)


getLiteral :: Char -> Char -> Bool -> C.ByteString -> String
getLiteral _  _  _ (C.uncons -> Nothing)  = []
getLiteral b e False ys@(C.uncons -> Just (x,xs))
    | x == b     =  b : getLiteral b e True xs
    | otherwise  = error $ "literal: error " ++ C.unpack ys
getLiteral b e True (C.uncons -> Just (x,xs))
    | x == e     = [e]
    | x == '\\'  = '\\' : x' : getLiteral b e True xs'
    | otherwise  = x : getLiteral b e True xs
                    where
                        (C.uncons -> Just(x',xs')) = xs
getLiteral _  _ _ _ = []


operOrPunct :: HS.HashSet String
operOrPunct =  HS.fromList [ "{","}","[","]","#","(",")",";",":","?",".","+","-","*",
                             "/","%","^","&","|","~","!","=","<",">","," ,
                             "##", "<:", ":>", "<%", "%>", "%:", "::", ".*", "+=", "-=",
                             "*=", "/=", "%=", "^=", "&=", "|=", "<<", ">>", ">=", "<=",
                             "&&", "||", "==", "!=", "++", "--", "->", "//", "/*", "*/",
                             "...", "<<=", ">>=", "->*",
                             "%:%:" ]

keywords :: HS.HashSet String
keywords = HS.fromList ["alignas", "continue", "friend", "alignof", "decltype", "goto", "asm",
                       "default", "if", "auto", "delete", "inline", "bool", "do", "int", "break",
                       "double", "long", "case", "dynamic_cast", "mutable", "catch", "else",
                       "namespace", "char", "enum", "new", "char16_t", "explicit", "noexcept",
                       "char32_t", "export", "nullptr", "class", "extern", "operator", "const",
                       "false", "private", "constexpr", "float", "protected", "const_cast", "for",
                       "public", "register", "true", "reinterpret_cast", "try", "return", "typedef",
                       "short", "typeid", "signed", "typename", "sizeof", "union", "static", "unsigned",
                       "static_assert", "using", "static_cast", "virtual", "struct", "void", "switch",
                       "volatile", "template", "wchar_t", "this", "while", "thread_local", "throw",
                       "and", "and_eq", "bitand", "bitor", "compl", "not", "not_eq", "or", "or_eq",
                       "xor", "xor_eq"]


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

module CGrep.Strategy.Cpp.Token(Token(..), TokenFilter(..), tokenizer, tokenFilter, 
                            isIdentifier, isKeyword, isDirective, isLiteralNumber, 
                            isHeaderName, isString, isChar, isOperOrPunct, tokenCompare
                            )  where
-- import Data.Int

import Data.Char 
import Data.Maybe
import Data.Set as S
import Data.Array 
import Control.Monad
-- import Debug.Trace

import qualified Data.ByteString.Char8 as C

type TokenizerState = (Source, Offset, Lineno, State)

type Source = C.ByteString

type Offset = Int
type Lineno = Int


-- Tokenize the source code in a list of Token 
-- Precondition: the c++ source code must be well-formed
--

tokenizer :: Source -> [Token]
tokenizer xs = runGetToken (ys, n, l, Null)  
            where (ys,n, l) = dropWhite xs


data TokenFilter = TokenFilter 
                   {
                        filtIdentifier :: Bool,     
                        filtDirective  :: Bool,
                        filtKeyword    :: Bool,
                        filtHeader     :: Bool,
                        filtString     :: Bool,
                        filtNumber     :: Bool,
                        filtChar       :: Bool,
                        filtOper       :: Bool

                   } deriving (Show,Read,Eq)


tokenFilter :: TokenFilter -> Token -> Bool

tokenFilter filt (TIdentifier{})  = filtIdentifier filt
tokenFilter filt (TDirective{})   = filtDirective  filt
tokenFilter filt (TKeyword{})     = filtKeyword    filt
tokenFilter filt (THeaderName{})  = filtHeader     filt
tokenFilter filt (TNumber{})      = filtNumber     filt
tokenFilter filt (TString{})      = filtString     filt
tokenFilter filt (TChar{})        = filtChar       filt
tokenFilter filt (TOperOrPunct{}) = filtOper       filt 


data Token = TIdentifier  { toString :: String, offset :: Int , lineno :: Int } |
             TDirective   { toString :: String, offset :: Int , lineno :: Int } |
             TKeyword     { toString :: String, offset :: Int , lineno :: Int } |
             TNumber      { toString :: String, offset :: Int , lineno :: Int } |
             THeaderName  { toString :: String, offset :: Int , lineno :: Int } |
             TString      { toString :: String, offset :: Int , lineno :: Int } |
             TChar        { toString :: String, offset :: Int , lineno :: Int } |
             TOperOrPunct { toString :: String, offset :: Int , lineno :: Int }
                deriving (Show, Eq)  

tokenCompare :: Token -> Token -> Bool
tokenCompare (TIdentifier { toString = l }) (TIdentifier { toString = r }) = l == r
tokenCompare (TDirective  { toString = l }) (TDirective  { toString = r }) = l == r
tokenCompare (TKeyword    { toString = l }) (TKeyword    { toString = r }) = l == r
tokenCompare (TNumber     { toString = l }) (TNumber     { toString = r }) = l == r
tokenCompare (THeaderName { toString = l }) (THeaderName { toString = r }) = l == r
tokenCompare (TString     { toString = l }) (TString     { toString = r }) = l == r
tokenCompare (TChar       { toString = l }) (TChar       { toString = r }) = l == r
tokenCompare (TOperOrPunct{ toString = l }) (TOperOrPunct{ toString = r }) = l == r
tokenCompare _ _ = False


isIdentifier :: Token -> Bool
isIdentifier (TIdentifier {})  = True
isIdentifier _ = False


isKeyword :: Token -> Bool
isKeyword (TKeyword {})  = True
isKeyword _ = False


isDirective :: Token -> Bool
isDirective (TDirective {})  = True
isDirective _ = False


isLiteralNumber :: Token -> Bool
isLiteralNumber (TNumber {}) = True
isLiteralNumber _ = False


isHeaderName :: Token -> Bool
isHeaderName (THeaderName {})  = True
isHeaderName _ = False


isString :: Token -> Bool
isString (TString {}) = True
isString _ = False


isChar :: Token -> Bool
isChar (TChar {}) = True
isChar _ = False


isOperOrPunct :: Token -> Bool
isOperOrPunct (TOperOrPunct {})  = True
isOperOrPunct _ = False


-- Drop leading whitespace and count them
--

dropWhite :: Source -> (Source, Offset, Lineno)
dropWhite xs = (xs', doff, dnl)
                where xs' = C.dropWhile (\c -> isSpace c || c == '\\') xs
                      doff = fromIntegral $ C.length xs - C.length xs'
                      dnl  = C.length $ C.filter (=='\n') (C.take doff xs)


data State = Null | Hash | Include | Define | Undef | If | Ifdef | Ifndef | Elif | Else | Endif |
                    Line | Error | Pragma
                    deriving (Show, Eq)


nextState :: String -> State -> State
nextState "#"               _    = Hash
nextState "include"         Hash = Include
nextState "include_next"    Hash = Include
nextState "define"          Hash = Define
nextState "undef"           Hash = Undef
nextState "if"              Hash = If 
nextState "ifdef"           Hash = Ifdef 
nextState "ifndef"          Hash = Ifndef 
nextState "elif"            Hash = Elif 
nextState "else"            Hash = Null 
nextState "endif"           Hash = Null 
nextState "line"            Hash = Line  
nextState "error"           Hash = Error  
nextState "pragma"          Hash = Pragma
nextState _  _  = Null

---

runGetToken :: TokenizerState -> [Token]

runGetToken (C.uncons  -> Nothing, _, _, _) = []
runGetToken tstate = token : runGetToken ns
    where (token, ns) = getToken tstate


getToken :: TokenizerState -> (Token, TokenizerState)

getToken (C.uncons -> Nothing, _, _, _) = error "getToken: internal error"
getToken (xs, off, ln, state) = let token = fromJust $ 
                                        getTokenDirective xs state       `mplus`
                                        getTokenHeaderName xs state      `mplus`
                                        getTokenNumber xs state          `mplus`
                                        getTokenIdOrKeyword xs state     `mplus`
                                        getTokenString xs state          `mplus`
                                        getTokenChar xs state            `mplus`
                                        getTokenOpOrPunct xs state
                                    len = fromIntegral $ length (toString token)
                                    (xs', w, n) = dropWhite $ C.drop (fromIntegral len) xs
                               in
                                   (token { offset = off, lineno = ln }, (xs', off + len + w, ln + n, nextState(toString token) state))


getTokenIdOrKeyword, getTokenNumber, getTokenHeaderName, 
     getTokenString, getTokenChar, getTokenOpOrPunct, getTokenDirective :: Source -> State -> Maybe Token


getTokenDirective xs  state 
    | state == Hash = Just (TDirective name 0 0)
    | otherwise = Nothing
                      where name = C.unpack $ C.takeWhile isIdentifierChar xs

getTokenHeaderName  xs@(C.uncons -> Just (x,_)) state 
    | state /= Include  = Nothing
    | x == '<'          = Just $ THeaderName (getLiteral '<'  '>'  False xs)   0 0
    | x == '"'          = Just $ THeaderName (getLiteral '"'  '"'  False xs)   0 0
    | otherwise         = Just $ THeaderName (C.unpack $ C.takeWhile isIdentifierChar xs) 0 0

getTokenHeaderName (C.uncons -> Nothing) _ = error "getTokenHeaderName: internal error"
getTokenHeaderName _ _ = error "getTokenHeaderName: internal error"


getTokenNumber ys@(C.uncons -> Just (x,_)) _   
    | x == '.' || isDigit x  = let ts = getNumber ys NumberNothing in 
                                case ts of 
                                    ""     -> Nothing 
                                    "."    -> Nothing 
                                    _      -> Just $ TNumber ts 0 0
    | otherwise = Nothing

getTokenNumber (C.uncons -> Nothing) _ = Nothing


validHexSet, validOctSet, validDecSet, validFloatSet :: S.Set Char

validHexSet   = S.fromList "0123456789abcdefABCDEFxXuUlL"
validOctSet   = S.fromList "01234567uUlL"
validDecSet   = S.fromList "0123456789uUlL"
validFloatSet = S.fromList "0123456789"

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
                                                | x `S.member` validHexSet -> x : getNumber xs NumberHex
                                                | x == '.'  -> x : getNumber xs NumberMayBeFloat
                                                | isDigit x -> x : getNumber xs NumberOct
                                                | otherwise -> ""

    |  state == NumberDec = case () of _
                                                | x `S.member` validDecSet -> x : getNumber xs NumberDec
                                                | x == '.'  -> x : getNumber xs NumberMayBeFloat
                                                | x == 'e' || x == 'E'  -> x : getNumber xs NumberExp
                                                | otherwise -> ""

    |  state == NumberOct = case () of _
                                                | x `S.member` validOctSet -> x : getNumber xs NumberOct
                                                | otherwise -> ""

    |  state == NumberHex = case () of _
                                                | x `S.member` validHexSet -> x : getNumber xs NumberHex
                                                | otherwise -> ""

    |  state == NumberMayBeFloat = case () of _
                                                | x `S.member` validDecSet   -> x : getNumber xs NumberFloat
                                                | otherwise                  -> ""
    
    |  state == NumberFloat = case () of _
                                                | x `S.member` validFloatSet -> x : getNumber xs NumberFloat
                                                | x == 'e' || x == 'E'       -> x : getNumber xs NumberExp
                                                | otherwise                  -> ""
    
    |  state == NumberExp = case () of _
                                                | x `S.member` validDecSet   -> x : getNumber xs NumberExp
                                                | x == '+' || x == '-'       -> x : getNumber xs NumberExp
                                                | otherwise                  -> ""

getNumber  _ _ = undefined


getTokenString xs@(C.uncons -> Just (x,_)) _
    | x == '"' = Just $ TString (getLiteral '"'  '"'  False xs) 0 0
    | otherwise = Nothing
getTokenString (C.uncons -> Nothing) _ = Nothing
getTokenString _ _ = Nothing


getTokenChar xs@(C.uncons -> Just (x,_)) _
    | x == '\'' = Just $ TChar  (getLiteral '\'' '\'' False xs) 0 0
    | otherwise = Nothing
getTokenChar (C.uncons -> Nothing) _ = Nothing
getTokenChar _ _ = Nothing


getTokenIdOrKeyword xs@(C.uncons -> Just (x,_)) _
    | not $ isIdentifierChar x = Nothing 
    | name `S.member` keywords = Just $ TKeyword name 0 0
    | otherwise                = Just $ TIdentifier name 0 0
                                    where name = C.unpack $ C.takeWhile isIdentifierChar xs
getTokenIdOrKeyword (C.uncons -> Nothing) _ = Nothing
getTokenIdOrKeyword _ _ = Nothing


getTokenOpOrPunct source _ = go source (min 4 (C.length source)) 
    where go _ 0   
            | C.length source > 0 = error $ "getTokenOpOrPunct: error near " ++ show source
            | otherwise = Nothing
          go src len 
            | sub `S.member` (operOrPunct ! fromIntegral len) = Just $ TOperOrPunct sub 0 0 
            | otherwise = go src (len-1)
                where sub = C.unpack (C.take len src)
                                                                                                              

getLiteral :: Char -> Char -> Bool -> C.ByteString -> String
getLiteral _  _  _ (C.uncons -> Nothing)  = []
getLiteral b e False ys@(C.uncons -> Just (x,xs))
    | x == b     =  b : getLiteral b e True xs
    | otherwise  = error $ "getLiteral: error near " ++ C.unpack ys 
getLiteral b e True (C.uncons -> Just (x,xs)) 
    | x == e     = [e]
    | x == '\\'  = '\\' : x' : getLiteral b e True xs' 
    | otherwise  = x : getLiteral b e True xs
                    where
                        (C.uncons -> Just(x',xs')) = xs
getLiteral _  _ _ _ = []



isIdentifierChar :: Char -> Bool 
isIdentifierChar c = isAlphaNum c || c == '_' || c == '$' -- GNU allows $ in identifiers 



operOrPunct :: Array Int (S.Set String) 
operOrPunct =  listArray (1, 4) [ S.fromList [ "{","}","[","]","#","(",")",";",":","?",".","+","-","*",
                                               "/","%","^","&","|","~","!","=","<",">","," ],
                                  S.fromList [ "##", "<:", ":>", "<%", "%>", "%:", "::", ".*", "+=", "-=", 
                                               "*=", "/=", "%=", "^=", "&=", "|=", "<<", ">>", ">=", "<=", 
                                               "&&", "||", "==", "!=", "++", "--", "->", "//", "/*", "*/"],      
                                  S.fromList [ "...", "<<=", ">>=", "->*"],   
                                  S.fromList [ "%:%:" ]]

keywords :: S.Set String
keywords = S.fromList ["alignas", "continue", "friend", "alignof", "decltype", "goto", "asm", 
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
                        

--
-- Copyright (c) 2013 Bonelli Nicola <bonelli@antifork.org>
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

{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE ViewPatterns #-} 

module CGrep.StringLike(StringLike(..)) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import Data.ByteString.Lazy.Search as LC

import Data.Function
import Data.String
import Data.Char
import Data.List

import Control.Monad (liftM)

import Util


data TokenState = TokenSpace |
                  TokenAlpha |
                  TokenDigit |
                  TokenOther 
                    deriving (Eq, Enum)


isCharNumber :: Char -> Bool
isCharNumber c = isHexDigit c || c == '.' || c == 'x' || c == 'X' 


-- | StringLike class
--                                            

class (IsString a) => StringLike a  where

    slToString    :: (IsString a) => a -> String

    slWords       :: (IsString a) => a -> [a]
    slLines       :: (IsString a) => a -> [a]
    slTokens      :: (IsString a) => a -> [a]
    
    slReadFile    :: (IsString a) => Bool -> FilePath -> IO a
    slGetContents :: (IsString a) => Bool -> IO a

    slSearch      :: (IsString a) => Bool -> [a] -> a -> [a]

    ciEqual       :: (IsString a) => a -> a -> Bool
    ciIsPrefixOf  :: (IsString a) => a -> a -> Bool
    ciIsInfixOf   :: (IsString a) => a -> a -> Bool


-- String

instance StringLike [Char] where

    slToString = id 

    slWords  = words
    slLines  = lines

    slTokens ys = tokens' (TokenSpace,"") ys
        where tokens' :: (TokenState, String) -> String -> [String]
              tokens' (TokenSpace, acc) (x:xs) =  
                  case () of
                    _  | isSpace x                ->  tokens' (TokenSpace, acc) xs
                       | isAlpha x || x == '_'    ->  tokens' (TokenAlpha, x : acc) xs
                       | isDigit x                ->  tokens' (TokenDigit, x : acc) xs
                       | otherwise                ->  tokens' (TokenOther, x : acc) xs
              
              tokens' (TokenAlpha, acc) (x:xs) = 
                  case () of
                    _  | isSpace x                ->  reverse acc : tokens' (TokenSpace, "") xs
                       | isAlphaNum x || x == '_' ->  tokens' (TokenAlpha, x : acc) xs
                       | otherwise                ->  reverse acc : tokens' (TokenOther, [x]) xs
              
              tokens' (TokenDigit, acc) (x:xs) = 
                  case () of
                    _  | isSpace x                ->  reverse acc : tokens' (TokenSpace, "") xs
                       | isCharNumber x           ->  tokens' (TokenDigit, x : acc) xs
                       | isAlpha x || x == '_'    ->  reverse acc : tokens' (TokenAlpha, [x]) xs
                       | otherwise                ->  reverse acc : tokens' (TokenOther, [x]) xs
              
              tokens' (TokenOther, acc) (x:xs) = 
                  case () of
                    _  | isSpace x                ->  reverse acc : tokens' (TokenSpace, "") xs
                       | isAlpha x || x == '_'    ->  reverse acc : tokens' (TokenAlpha, [x]) xs
                       | isDigit x                ->  if acc == "." then tokens' (TokenDigit, x : ".") xs
                                                                    else reverse acc : tokens' (TokenDigit, [x]) xs
                       | otherwise                ->  tokens' (TokenOther, x : acc) xs
              tokens' (_, acc) _ =  [acc] 

    slSearch wordmatch patterns s 
        | wordmatch  = let ws = slTokens s in filter (\p -> (p `isInfixOf` s) && (p `elem` ws)) patterns   
        | otherwise  = filter (`isInfixOf` s) patterns   

    slReadFile ignoreCase f 
        | ignoreCase = liftM (map toLower) $ readFile f 
        | otherwise  = readFile f

    slGetContents ignoreCase 
        | ignoreCase = liftM (map toLower) getContents
        | otherwise  = getContents


    ciEqual =  (==) `on` map toLower 
    
    ciIsPrefixOf [] _           =  True
    ciIsPrefixOf _  []          =  False
    ciIsPrefixOf (x:xs) (y:ys)  =  toLower x == toLower y && ciIsPrefixOf xs ys
    
    ciIsInfixOf needle haystack = any (ciIsPrefixOf needle) (tails haystack)

-- C.ByteString

instance StringLike C.ByteString where

    slToString = C.unpack 

    slWords  = C.words 
    slLines  = C.lines

    slTokens ys = tokens' (TokenSpace, C.empty) ys
        where tokens' :: (TokenState, C.ByteString) -> C.ByteString -> [C.ByteString]
              tokens' (TokenSpace, acc) (C.uncons -> Just (x,xs)) =  
                  case () of
                    _  | isSpace x                ->  tokens' (TokenSpace, acc) xs
                       | isAlpha x || x == '_'    ->  tokens' (TokenAlpha, x `C.cons` acc) xs
                       | isDigit x                ->  tokens' (TokenDigit, x `C.cons` acc) xs
                       | otherwise                ->  tokens' (TokenOther, x `C.cons` acc) xs
              
              tokens' (TokenAlpha, acc) (C.uncons -> Just (x,xs)) = 
                  case () of
                    _  | isSpace x                ->  C.reverse acc : tokens' (TokenSpace, C.empty) xs
                       | isAlphaNum x || x == '_' ->  tokens' (TokenAlpha, x `C.cons` acc) xs
                       | otherwise                ->  C.reverse acc : tokens' (TokenOther, C.singleton x) xs
              
              tokens' (TokenDigit, acc) (C.uncons -> Just (x,xs)) = 
                  case () of
                    _  | isSpace x                ->  C.reverse acc : tokens' (TokenSpace, C.empty) xs
                       | isCharNumber x           ->  tokens' (TokenDigit, x `C.cons` acc) xs
                       | isAlpha x || x == '_'    ->  C.reverse acc : tokens' (TokenAlpha, C.singleton x) xs
                       | otherwise                ->  C.reverse acc : tokens' (TokenOther, C.singleton x) xs
              
              tokens' (TokenOther, acc) (C.uncons -> Just (x,xs)) = 
                  case () of
                    _  | isSpace x                ->  C.reverse acc : tokens' (TokenSpace, C.empty) xs
                       | isAlpha x || x == '_'    ->  C.reverse acc : tokens' (TokenAlpha, C.singleton x) xs
                       | isDigit x                ->  if acc == C.pack "." then tokens' (TokenDigit, x `C.cons` C.singleton '.') xs
                                                                           else C.reverse acc : tokens' (TokenDigit, C.singleton x) xs
                       | otherwise                ->  tokens' (TokenOther, x `C.cons` acc) xs
              tokens' (_, acc) _ =  [acc] 

    slSearch wordmatch patterns s 
        | wordmatch = let ws = slTokens s in filter (\p -> (p `C.isInfixOf` s) && (p `elem` ws)) patterns   
        | otherwise = filter (`C.isInfixOf` s) patterns   

    slReadFile ignoreCase f
        | ignoreCase = liftM (C.map toLower) $ C.readFile f
        | otherwise  = C.readFile f

    slGetContents ignoreCase 
        | ignoreCase = liftM (C.map toLower) C.getContents
        | otherwise  = C.getContents

    ciEqual = (==) `on` C.map toLower 

    ciIsPrefixOf (C.uncons -> Nothing) _   =  True
    ciIsPrefixOf _  (C.uncons -> Nothing)  =  False
    ciIsPrefixOf (C.uncons -> Just (x,xs)) (C.uncons -> Just (y,ys))  =  toLower x == toLower y && ciIsPrefixOf xs ys 
    ciIsPrefixOf _ _ = undefined

    ciIsInfixOf needle haystack = any (ciIsPrefixOf needle) (C.tails haystack)


instance StringLike LC.ByteString where

    slToString = LC.unpack 
    
    slWords  = LC.words
    slLines  = LC.lines

    slTokens ys = tokens' (TokenSpace, LC.empty) ys
        where tokens' :: (TokenState, LC.ByteString) -> LC.ByteString -> [LC.ByteString]
              tokens' (TokenSpace, acc) (LC.uncons -> Just (x,xs)) =  
                  case () of
                    _  | isSpace x                ->  tokens' (TokenSpace, acc) xs
                       | isAlpha x || x == '_'    ->  tokens' (TokenAlpha, x `LC.cons` acc) xs
                       | isDigit x                ->  tokens' (TokenDigit, x `LC.cons` acc) xs
                       | otherwise                ->  tokens' (TokenOther, x `LC.cons` acc) xs
              
              tokens' (TokenAlpha, acc) (LC.uncons -> Just (x,xs)) = 
                  case () of
                    _  | isSpace x                ->  LC.reverse acc : tokens' (TokenSpace, LC.empty) xs
                       | isAlphaNum x || x == '_' ->  tokens' (TokenAlpha, x `LC.cons` acc) xs
                       | otherwise                ->  LC.reverse acc : tokens' (TokenOther, LC.singleton x) xs
              
              tokens' (TokenDigit, acc) (LC.uncons -> Just (x,xs)) = 
                  case () of
                    _  | isSpace x                ->  LC.reverse acc : tokens' (TokenSpace, LC.empty) xs
                       | isCharNumber x           ->  tokens' (TokenDigit, x `LC.cons` acc) xs
                       | isAlpha x || x == '_'    ->  LC.reverse acc : tokens' (TokenAlpha, LC.singleton x) xs
                       | otherwise                ->  LC.reverse acc : tokens' (TokenOther, LC.singleton x) xs
              
              tokens' (TokenOther, acc) (LC.uncons -> Just (x,xs)) = 
                  case () of
                    _  | isSpace x                ->  LC.reverse acc : tokens' (TokenSpace, LC.empty) xs
                       | isAlpha x || x == '_'    ->  LC.reverse acc : tokens' (TokenAlpha, LC.singleton x) xs
                       | isDigit x                ->  if acc == LC.pack "." then tokens' (TokenDigit, x `LC.cons` LC.singleton '.') xs
                                                                           else LC.reverse acc : tokens' (TokenDigit, LC.singleton x) xs
                       | otherwise                ->  tokens' (TokenOther, x `LC.cons` acc) xs
              tokens' (_, acc) _ =  [acc] 


    slSearch wordmatch patterns s 
        | wordmatch = let ws = slTokens s in filter (`elem` ws) patterns   
        | otherwise = map ((patterns!!) . snd) $ filter (\p -> notNull (LC.indices (fst p) s)) (zip (map toStrict patterns) [0..])

    slReadFile ignoreCase f
        | ignoreCase = liftM (LC.map toLower) $ LC.readFile f 
        | otherwise  = LC.readFile f

    slGetContents ignoreCase 
        | ignoreCase = liftM (LC.map toLower) LC.getContents
        | otherwise  = LC.getContents
    

    ciEqual = (==) `on` LC.map toLower 

    ciIsPrefixOf (LC.uncons -> Nothing) _  =  True
    ciIsPrefixOf _  (LC.uncons -> Nothing) =  False
    ciIsPrefixOf (LC.uncons -> Just (x,xs)) (LC.uncons -> Just (y,ys))  =  toLower x == toLower y && ciIsPrefixOf xs ys
    ciIsPrefixOf _ _ = undefined
    
    ciIsInfixOf needle haystack = any (ciIsPrefixOf needle) (LC.tails haystack)



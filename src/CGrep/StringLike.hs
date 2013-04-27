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

module CGrep.StringLike(StringLike(..), toStrict) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

-- import Data.ByteString.Search as C
import Data.ByteString.Lazy.Search as LC

import Data.Function
import Data.String
import Data.Char
import Data.List

import Control.Monad (liftM)


xor :: Bool -> Bool -> Bool
a `xor` b = a && not b || not a && b


toStrict :: LC.ByteString -> C.ByteString
toStrict = C.concat . LC.toChunks


-- | StringLike class
--                                            

class (IsString a) => StringLike a  where

    slToString    :: (IsString a) => a -> String

    slReadFile    :: (IsString a) => Bool -> FilePath -> IO a
    slGetContents :: (IsString a) => Bool -> IO a

    slWords       :: (IsString a) => a -> [a]
    slGrep        :: (IsString a) => Bool -> Bool -> [a] -> a -> [a]

    ciEqual       :: (IsString a) => a -> a -> Bool
    ciIsPrefixOf  :: (IsString a) => a -> a -> Bool
    ciIsInfixOf   :: (IsString a) => a -> a -> Bool


instance StringLike [Char] where

    slToString = id 

    slWords s = case dropWhile isSpace' s of                 
                "" -> []                                 
                s' -> w : slWords s''                    
                    where (w, s'') = break isSpace' s'    
               where isSpace' c = notElem c $ '_' : '$' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] 
    
    slGrep wordmatch invert patterns s 
        | wordmatch  = let ws = slWords s in filter (\p -> ((p `isInfixOf` s) && (p `elem` ws)) `xor` invert) patterns   
        | otherwise  = filter (\p -> (p `isInfixOf` s) `xor` invert) patterns   

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


instance StringLike C.ByteString where

    slToString = C.unpack 

    slWords = filter (not . C.null) . C.splitWith (`notElem` '_' : '$' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) 

    slGrep wordmatch invert patterns s 
        | wordmatch = let ws = slWords s in filter (\p -> ((p `C.isInfixOf` s) && (p `elem` ws)) `xor` invert) patterns   
        | otherwise = filter (\p -> (p `C.isInfixOf` s) `xor` invert) patterns   

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
    
    slWords s = case LC.dropWhile isSpace' s of                 
                (LC.uncons -> Nothing) -> []                                 
                s' -> w : slWords s''                    
                    where (w, s'') = LC.break isSpace' s'    
               where isSpace' c = notElem c $ '_' : '$' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] 


    slGrep wordmatch invert patterns s 
        | wordmatch = let ws = slWords s in filter (\p -> (p `elem` ws) `xor` invert) patterns   
        | otherwise = map ((patterns!!) . snd) $ filter (\p -> (not . null $ LC.indices (fst p) s) `xor` invert) (zip (map toStrict patterns) [0..])

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



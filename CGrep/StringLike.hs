{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE ViewPatterns #-} 

module CGrep.StringLike where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import Data.Function
import Data.String
import Data.Char
import Data.List

-- | StringLike class
--

class StringLike a  where

    gwords       :: (IsString a) => a -> [a]
    ciEqual      :: (IsString a) => a -> a -> Bool
    ciIsPrefixOf :: (IsString a) => a -> a -> Bool
    ciIsInfixOf  :: (IsString a) => a -> a -> Bool


instance StringLike [Char] where

    gwords s = case dropWhile isSpace' s of                 
                "" -> []                                 
                s' -> w : gwords s''                    
                    where (w, s'') = break isSpace' s'    
               where isSpace' c = notElem c $ '_' : '$' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] 
    
    ciEqual =  (==) `on` map toLower 
    
    ciIsPrefixOf [] _           =  True
    ciIsPrefixOf _  []          =  False
    ciIsPrefixOf (x:xs) (y:ys)  =  toLower x == toLower y && ciIsPrefixOf xs ys
    
    ciIsInfixOf needle haystack = any (ciIsPrefixOf needle) (tails haystack)


instance StringLike C.ByteString where
    gwords s = case C.dropWhile isSpace' s of                 
                (C.uncons -> Nothing) -> []                                 
                s' -> w : gwords s''                    
                    where (w, s'') = C.break isSpace' s'    
               where isSpace' c = notElem c $ '_' : '$' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] 

    ciEqual = (==) `on` C.map toLower 

    ciIsPrefixOf (C.uncons -> Nothing) _   =  True
    ciIsPrefixOf _  (C.uncons -> Nothing)  =  False
    ciIsPrefixOf (C.uncons -> Just (x,xs)) (C.uncons -> Just (y,ys))  =  toLower x == toLower y && ciIsPrefixOf xs ys 
    ciIsPrefixOf _ _ = undefined

    ciIsInfixOf needle haystack = any (ciIsPrefixOf needle) (C.tails haystack)


instance StringLike LC.ByteString where
    gwords s = case LC.dropWhile isSpace' s of                 
                (LC.uncons -> Nothing) -> []                                 
                s' -> w : gwords s''                    
                    where (w, s'') = LC.break isSpace' s'    
               where isSpace' c = notElem c $ '_' : '$' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] 

    ciEqual = (==) `on` LC.map toLower 

    ciIsPrefixOf (LC.uncons -> Nothing) _  =  True
    ciIsPrefixOf _  (LC.uncons -> Nothing) =  False
    ciIsPrefixOf (LC.uncons -> Just (x,xs)) (LC.uncons -> Just (y,ys))  =  toLower x == toLower y && ciIsPrefixOf xs ys
    ciIsPrefixOf _ _ = undefined
    
    ciIsInfixOf needle haystack = any (ciIsPrefixOf needle) (LC.tails haystack)



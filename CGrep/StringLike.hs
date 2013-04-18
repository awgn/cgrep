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


-- | StringLike class
--                                            


xor :: Bool -> Bool -> Bool
a `xor` b = a && not b || not a && b


toStrict :: LC.ByteString -> C.ByteString
toStrict = C.concat . LC.toChunks


class (IsString a) => StringLike a  where

    toString     :: (IsString a) => a -> String
    gwords       :: (IsString a) => a -> [a]
    grep         :: (IsString a) => Bool -> Bool -> Bool -> [a] -> a -> [a]
    ciEqual      :: (IsString a) => a -> a -> Bool
    ciIsPrefixOf :: (IsString a) => a -> a -> Bool
    ciIsInfixOf  :: (IsString a) => a -> a -> Bool


instance StringLike [Char] where

    toString a = a :: String

    gwords s = case dropWhile isSpace' s of                 
                "" -> []                                 
                s' -> w : gwords s''                    
                    where (w, s'') = break isSpace' s'    
               where isSpace' c = notElem c $ '_' : '$' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] 
    
    grep wordmatch ignorecase invert patterns s 
        | not wordmatch && not ignorecase = filter (\p -> (p `isInfixOf` s) `xor` invert) patterns   
        | wordmatch     && not ignorecase = let ws = gwords s in filter (\p -> ((p `isInfixOf` s) && (p `elem` ws)) `xor` invert) patterns   
        | not wordmatch &&     ignorecase = filter (\p -> (p `ciIsInfixOf` s) `xor` invert) patterns   
        | otherwise                       = let ws = gwords s in filter (\p -> any (p `ciEqual`) ws `xor` invert) patterns   


    ciEqual =  (==) `on` map toLower 
    
    ciIsPrefixOf [] _           =  True
    ciIsPrefixOf _  []          =  False
    ciIsPrefixOf (x:xs) (y:ys)  =  toLower x == toLower y && ciIsPrefixOf xs ys
    
    ciIsInfixOf needle haystack = any (ciIsPrefixOf needle) (tails haystack)


instance StringLike C.ByteString where

    toString = C.unpack 

    gwords = filter (not . C.null) . C.splitWith (`notElem` '_' : '$' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) 

    -- gwords s = case C.dropWhile isSpace' s of                 
    --             (C.uncons -> Nothing) -> []                                 
    --             s' -> w : gwords s''                    
    --                 where (w, s'') = C.break isSpace' s'    
    --            where isSpace' c = notElem c $ '_' : '$' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] 

    grep wordmatch ignorecase invert patterns s 
        | not wordmatch && not ignorecase = filter (\p -> (p `C.isInfixOf` s) `xor` invert) patterns   
        | wordmatch     && not ignorecase = let ws = gwords s in filter (\p -> ((p `C.isInfixOf` s) && (p `elem` ws)) `xor` invert) patterns   
        | not wordmatch &&     ignorecase = filter (\p -> (p `ciIsInfixOf` s) `xor` invert) patterns   
        | otherwise                       = let ws = gwords s in filter (\p -> (p `ciIsInfixOf` s && any (p `ciEqual`) ws) `xor` invert) patterns   
    
    ciEqual = (==) `on` C.map toLower 

    ciIsPrefixOf (C.uncons -> Nothing) _   =  True
    ciIsPrefixOf _  (C.uncons -> Nothing)  =  False
    ciIsPrefixOf (C.uncons -> Just (x,xs)) (C.uncons -> Just (y,ys))  =  toLower x == toLower y && ciIsPrefixOf xs ys 
    ciIsPrefixOf _ _ = undefined

    ciIsInfixOf needle haystack = any (ciIsPrefixOf needle) (C.tails haystack)


instance StringLike LC.ByteString where

    toString = LC.unpack 
    
    gwords s = case LC.dropWhile isSpace' s of                 
                (LC.uncons -> Nothing) -> []                                 
                s' -> w : gwords s''                    
                    where (w, s'') = LC.break isSpace' s'    
               where isSpace' c = notElem c $ '_' : '$' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] 


    grep wordmatch ignorecase invert patterns s 
        | not wordmatch && not ignorecase = map ((patterns!!) . snd) $ filter (\p -> (not . null $ LC.indices (fst p) s) `xor` invert) (zip (map toStrict patterns) [0..])
        | wordmatch     && not ignorecase = let ws = gwords s in filter (\p -> (p `elem` ws) `xor` invert) patterns   
        | not wordmatch &&     ignorecase = filter (\p -> (p `ciIsInfixOf` s) `xor` invert) patterns   
        | otherwise                       = let ws = gwords s in filter (\p -> any (p `ciEqual`) ws `xor` invert) patterns   

    ciEqual = (==) `on` LC.map toLower 

    ciIsPrefixOf (LC.uncons -> Nothing) _  =  True
    ciIsPrefixOf _  (LC.uncons -> Nothing) =  False
    ciIsPrefixOf (LC.uncons -> Just (x,xs)) (LC.uncons -> Just (y,ys))  =  toLower x == toLower y && ciIsPrefixOf xs ys
    ciIsPrefixOf _ _ = undefined
    
    ciIsInfixOf needle haystack = any (ciIsPrefixOf needle) (LC.tails haystack)



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

module CGrep.Strategy.Semantic (cgrepCppSemantic) where

import qualified Data.ByteString.Char8 as C

import CGrep.Function
import CGrep.Output
import CGrep.StringLike
import CGrep.Filter 
import CGrep.Lang
import CGrep.Common

import Data.List
import Data.Function
import qualified Data.Map.Strict as Map

import Options 
import Util
import Debug

import qualified CGrep.Strategy.Cpp.Token  as Cpp


cgrepCppSemantic :: CgrepFunction
cgrepCppSemantic opt ps f = do

    putStrLevel1 (debug opt) $ "strategy  : running C/C++ semantic parser on " ++ f ++ "..."


    source <- if f == "" then slGetContents (ignore_case opt)  
                         else slReadFile (ignore_case opt) f

    let filtered = filterContext (lookupLang f) (mkContextFilter opt) source
    
    -- parse source code, get the Cpp.Token list...
    --
    
    let all_ts = Cpp.tokenizer filtered

    
    let tmatches  =  sortBy (compare `on` Cpp.offset) $ nub $ tokensMatch opt f lpt all_ts 
   

    let matches = map (\t -> let n = fromIntegral (Cpp.lineno t) in (n+1, [])) tmatches :: [(Int, [String])]

    putStrLevel2 (debug opt) $ "tokens :  " ++ show all_ts
    putStrLevel2 (debug opt) $ "patterns: " ++ show lpt
    putStrLevel2 (debug opt) $ "matches: "  ++ show matches 

    putStrLevel3 (debug opt) $ "---\n" ++ C.unpack filtered ++ "\n---"
    
    return $ mkOutput f source matches
        
        where  lpt = map (Cpp.tokenizer . filterContext (Just Cpp) sourceCodeFilter) ps
    

sourceCodeFilter :: ContextFilter 
sourceCodeFilter = ContextFilter { getCode = True, getComment = False, getLiteral = True }   


tokensMatch :: Options -> FilePath -> [[Cpp.Token]] -> [Cpp.Token] -> [Cpp.Token]
-- tokensMatch opt _ ps | trace ("ps = " ++ (show ps)) False = undefined 
tokensMatch _ _ [] _ = []
tokensMatch opt f (g:gs) ts = map (ts !!) (findIndices (groupCompare (word opt, invert_match opt) g) tokenGroups) ++ tokensMatch opt f gs ts 
    where tokenGroups = spanGroup (length g) ts


type WordMatch   = Bool
type InvertMatch = Bool
type Pattern     = Cpp.Token


groupCompare :: (WordMatch,InvertMatch) -> [Pattern] -> [Cpp.Token] -> Bool
groupCompare (wordmatch,invert) l r = groupCompareMatch ts &&
                                      groupCompareSemantic ts
        where ts = tokensGroupCompare (wordmatch,invert) l r               


{-# INLINE groupCompareMatch #-}

groupCompareMatch :: [(Bool, (String, [String]))] -> Bool
groupCompareMatch = all fst 

{-# INLINE groupCompareSemantic #-}

-- Note: pattern $ and _ match any token, whereas $1 $2 (_1 _2 etc.) match tokens
--       that must compare equal in corresponding occurrences  
--       

groupCompareSemantic :: [(Bool, (String, [String]))] -> Bool
-- groupCompareSemantic xs | trace ("semantic: " ++ show xs) False = undefined
groupCompareSemantic ts =  Map.foldr (\xs r -> r && all (== head xs) xs) True m  
        where m =  Map.mapWithKey (\k xs -> if k `elem` ["_","$","000"]
                                              then []
                                              else xs ) $ Map.fromListWith (++) (map snd ts)
        

tokensGroupCompare :: (WordMatch,InvertMatch) -> [Pattern] -> [Cpp.Token] -> [(Bool, (String, [String]))]
tokensGroupCompare (wordmatch,invert) l r 
    | length r >= length l = zipWith (tokensCompare (wordmatch,invert)) l r
    | otherwise = [ (False, ("", [])) ]


tokensCompare :: (WordMatch, InvertMatch) -> Pattern -> Cpp.Token -> (Bool,(String, [String]))

tokensCompare (True, invert) (Cpp.TIdentifier { Cpp.toString = l }) (Cpp.TIdentifier { Cpp.toString = r }) 
        | isWildCard l =  (invert `xor` patternMatch True l r,  (l,[r]))
        | otherwise    =  (invert `xor` (l == r), ("", []))

tokensCompare (False, invert) (Cpp.TIdentifier { Cpp.toString = l }) (Cpp.TIdentifier { Cpp.toString = r }) 
        | isWildCard l =  (invert `xor` patternMatch False l r, (l,[r]))
        | otherwise    =  (invert `xor` (l `isInfixOf` r) , ("", []))

tokensCompare (True, invert) (Cpp.TNumber { Cpp.toString = l }) (Cpp.TNumber { Cpp.toString = r }) 
        | isWildCard l =  (invert `xor` patternMatch True l r,  (l,[r]))
        | otherwise    =  (invert `xor` (l == r), ("", []))

tokensCompare (False, invert) (Cpp.TNumber { Cpp.toString = l }) (Cpp.TNumber { Cpp.toString = r }) 
        | isWildCard l =  (invert `xor` patternMatch False l r, (l,[r]))
        | otherwise    =  (invert `xor` (l `isInfixOf` r) , ("", []))

tokensCompare (wordmatch, invert) l r   
        | wordmatch   =  (invert `xor` Cpp.tokenCompare l r, ("", [])) 
        | otherwise   =  (invert `xor` Cpp.tokenCompare l r, ("",[]))

 
{-# INLINE isWildCard #-}

isWildCard :: String -> Bool
isWildCard s | ('_':_) <- s  = True
             | ('$':_) <- s  = True
             | s == "000"    = True
             | otherwise     = False

{-# INLINE patternMatch #-}

patternMatch :: WordMatch -> String -> String -> Bool
patternMatch _ ('_':_) _ = True
patternMatch _ ('$':_) _ = True
patternMatch _ "000"   _ = True
patternMatch _ _ _       = undefined 
    


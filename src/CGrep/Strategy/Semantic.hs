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
import qualified Data.Map.Strict as M

import CGrep.Filter 
import CGrep.Lang
import CGrep.Common

import Data.Maybe
import Data.List
import Data.Function

import Options 
import Debug

import qualified CGrep.Strategy.Cpp.Token  as Cpp


-- shortcuts for wildcard tokens...

ppKeywords :: M.Map String String
ppKeywords = M.fromList 
           [ ("ANY", "TOKEN_ANY"),
             ("KEY", "TOKEN_KEYWORD"),
             ("STR", "TOKEN_STRING"),
             ("CHR", "TOKEN_CHAR"),
             ("NUM", "TOKEN_NUMBER") ]


preprocToken :: Cpp.Token -> Cpp.Token
preprocToken t@Cpp.TIdentifier { Cpp.toString = str } = t { Cpp.toString = M.findWithDefault str str ppKeywords } 
preprocToken t = t


cgrepCppSemantic :: CgrepFunction
cgrepCppSemantic opt ps f = do
    
    source <- getText (ignore_case opt) f 
    
    let filename = if f == Nothing then "<stdin>" 
                                   else fromJust f


    putStrLevel1 (debug opt) $ "strategy  : running C/C++ semantic parser on " ++ filename ++ "..."


    let filtered = filterContext (lookupLang filename) (mkContextFilter opt) source
    
    -- parse source code, get the Cpp.Token list...
    --
    
    let sourceTokens = Cpp.tokenizer filtered

    -- preprocess shortcuts
    --

    let patTokens = map (Cpp.tokenizer . filterContext (Just Cpp) sourceCodeFilter) ps

    let patTokens' = map (map preprocToken) patTokens

    let extendedTokens = extendPatterns patTokens'

    let matchingTokens =  filterMatchingTokens opt filename extendedTokens sourceTokens 

    putStrLevel2 (debug opt) $ "patterns: " ++ show extendedTokens
    putStrLevel2 (debug opt) $ "matchingToken: " ++ show matchingTokens

    let tokenMatches = sortBy (compare `on` Cpp.offset) $ nub matchingTokens

    let matches = map (\t -> let n = fromIntegral (Cpp.lineno t) in (n+1, [Cpp.toString t])) tokenMatches :: [(Int, [String])]

    putStrLevel2 (debug opt) $ "matches: "  ++ show matches 

    putStrLevel3 (debug opt) $ "---\n" ++ C.unpack filtered ++ "\n---"
    
    return $ mkOutput opt filename source (mergeMatches matches)
        

type WordMatch   = Bool
type Pattern     = Cpp.Token


sourceCodeFilter :: ContextFilter 
sourceCodeFilter = ContextFilter { getCode = True, getComment = False, getLiteral = True }   


-- extend patterns for optional tokens ($)...

extendPatterns :: [[Pattern]] -> [[Pattern]]
extendPatterns = concatMap getPatternSubsequence 


getPatternSubsequence :: [Pattern] -> [[Pattern]]
getPatternSubsequence ps = map (`filterIndicies` ps') idx
    where ps' = zip [0..] ps
          idx = subsequences $ findIndices (\t -> case t of 
                                                   Cpp.TIdentifier { Cpp.toString = ('$':_) } -> True  
                                                   _ -> False) ps

filterIndicies :: [Int] -> [(Int,Pattern)] -> [Pattern]
filterIndicies ns ps = map snd $ filter (\(n, _) -> n `notElem` ns) ps


-- search for matching tokens...

filterMatchingTokens :: Options -> FilePath -> [[Pattern]] -> [Cpp.Token] -> [Cpp.Token]
-- filterMatchingTokens opt _ ps | trace ("ps = " ++ (show ps)) False = undefined 
filterMatchingTokens _ _ [] _ = []
filterMatchingTokens opt f (g:gs) ts = concatMap (take groupLen . (`drop` ts)) (findIndices (groupCompare (word_match opt) g) tokenGroups) ++ filterMatchingTokens opt f gs ts 
    where tokenGroups = spanGroup groupLen ts
          groupLen    = length g


groupCompare :: WordMatch -> [Pattern] -> [Cpp.Token] -> Bool
groupCompare wordmatch l r = groupCompareMatch ts &&
                             groupCompareSemantic ts
        where ts = tokensGroupCompare wordmatch l r               


{-# INLINE groupCompareMatch #-}

groupCompareMatch :: [(Bool, (String, [String]))] -> Bool
groupCompareMatch = all fst 

{-# INLINE groupCompareSemantic #-}

-- Note: pattern $ and _ match any token, whereas $1 $2 (_1 _2 etc.) match tokens
--       that must compare equal in corresponding occurrences  
--       

groupCompareSemantic :: [(Bool, (String, [String]))] -> Bool
-- groupCompareSemantic xs | trace ("semantic: " ++ show xs) False = undefined
groupCompareSemantic ts =  M.foldr (\xs r -> r && all (== head xs) xs) True m  
        where m =  M.mapWithKey (\k xs -> if k `elem` ["_","$", "TOKEN_ANY", "TOKEN_NUMBER", "TOKEN_KEYWORD", "TOKEN_STRING", "TOKEN_CHAR"]
                                              then []
                                              else xs ) $ M.fromListWith (++) (map snd ts)
        

tokensGroupCompare :: WordMatch -> [Pattern] -> [Cpp.Token] -> [(Bool, (String, [String]))]
tokensGroupCompare wordmatch l r 
    | length r >= length l = zipWith (tokensCompare wordmatch) l r
    | otherwise = [ (False, ("", [])) ]


tokensCompare :: WordMatch -> Pattern -> Cpp.Token -> (Bool,(String, [String]))

tokensCompare wm (Cpp.TIdentifier { Cpp.toString = l }) (Cpp.TIdentifier { Cpp.toString = r }) 
        | case l of 
            "TOKEN_ANY" -> True
            ('_':_) -> True 
            ('$':_) -> True
            _       -> False = (True, (l,[r]))
        | otherwise    =  if wm then (l == r, ("", []))
                                else (l `isInfixOf` r, ("", [])) 

tokensCompare _ (Cpp.TIdentifier { Cpp.toString = l }) (Cpp.TNumber { Cpp.toString = r }) 
        =  (l == "TOKEN_NUMBER"  || l == "TOKEN_ANY", (l,[r]))

tokensCompare _ (Cpp.TIdentifier { Cpp.toString = l }) (Cpp.TKeyword { Cpp.toString = r }) 
        =  (l == "TOKEN_KEYWORD" || l == "TOKEN_ANY", (l,[r]))

tokensCompare _ (Cpp.TIdentifier { Cpp.toString = l }) (Cpp.TString { Cpp.toString = r }) 
        =  (l == "TOKEN_STRING"  || l == "TOKEN_ANY", (l,[r]))

tokensCompare _ (Cpp.TIdentifier { Cpp.toString = l }) (Cpp.TChar { Cpp.toString = r }) 
        =  (l == "TOKEN_CHAR" || l == "TOKEN_ANY", (l,[r]))

tokensCompare _ (Cpp.TIdentifier { Cpp.toString = l }) rt
        =  (l == "TOKEN_ANY", (l,[Cpp.toString rt]))

tokensCompare _ l r  = (Cpp.tokenCompare l r, ("", [])) 

 

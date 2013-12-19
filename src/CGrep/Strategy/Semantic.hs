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

module CGrep.Strategy.Semantic (searchSemantic) where

import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M

import CGrep.Filter 
import CGrep.Lang
import CGrep.Common

import Data.Char
import Data.List
import Data.Function

import Options 
import Debug

import qualified CGrep.Strategy.Cpp.Token  as Cpp


searchSemantic :: CgrepFunction
searchSemantic opt ps f = do
    
    let filename = getFileName f 
    
    text <- getText (ignore_case opt) f 

    -- transform text
    
    let text' = getMultiLine (multiline opt) . contextFilter (lookupLang filename) ((mkContextFilter opt) { getComment = False} ) $ text


    -- parse source code, get the Cpp.Token list...
    --
    
    let tokens = Cpp.tokenizer text'

    -- pre-process patterns
    --

    let patterns  = map (Cpp.tokenizer . contextFilter (Just Cpp) sourceCodeFilter) ps


    let patterns' = map (map preprocToken) patterns >>= getPatternSubsequence


    -- get matching tokens ...
        
    let tokens' = sortBy (compare `on` Cpp.offset) $ nub (filterMatchingTokens opt filename patterns' tokens)   


    let matches = map (\t -> let n = fromIntegral (Cpp.offset t) in (n, Cpp.toString t)) tokens' :: [(Int, String)]


    putStrLevel1 (debug opt) $ "strategy  : running C/C++ semantic search on " ++ filename ++ "..."
    putStrLevel2 (debug opt) $ "patterns  : " ++ show patterns'
    putStrLevel2 (debug opt) $ "tokens    : " ++ show tokens'
    putStrLevel2 (debug opt) $ "matches   : " ++ show matches 
    putStrLevel3 (debug opt) $ "---\n" ++ C.unpack text' ++ "\n---"
    
    return $ mkOutput opt filename text' matches
        

type WordMatch   = Bool
type Pattern     = Cpp.Token

-- shortcuts for wildcard in patterns...

ppKeywords :: M.Map String String
ppKeywords = M.fromList 
           [ ("ANY", "TOKEN_ANY"),
             ("KEY", "TOKEN_KEYWORD"),
             ("STR", "TOKEN_STRING"),
             ("CHR", "TOKEN_CHAR"),
             ("NUM", "TOKEN_NUMBER"),
             ("OCT", "TOKEN_OCT"),
             ("HEX", "TOKEN_HEX")]


preprocToken :: Cpp.Token -> Cpp.Token
preprocToken t@Cpp.TokenIdentifier { Cpp.toString = str } = t { Cpp.toString = M.findWithDefault str str ppKeywords } 
preprocToken t = t


sourceCodeFilter :: ContextFilter 
sourceCodeFilter = ContextFilter { getCode = True, getComment = False, getLiteral = True }   


getPatternSubsequence :: [Pattern] -> [[Pattern]]
getPatternSubsequence ps = map (`filterIndicies` ps') idx
    where ps' = zip [0..] ps
          idx = subsequences $ findIndices (\t -> case t of 
                                                   Cpp.TokenIdentifier { Cpp.toString = ('$':_) } -> True  
                                                   _ -> False) ps


filterIndicies :: [Int] -> [(Int,Pattern)] -> [Pattern]
filterIndicies ns ps = map snd $ filter (\(n, _) -> n `notElem` ns) ps


filterMatchingTokens :: Options -> FilePath -> [[Pattern]] -> [Cpp.Token] -> [Cpp.Token]
filterMatchingTokens _ _ [] _ = []
filterMatchingTokens opt f (g:gs) ts = 
    concatMap (take grpLen . (`drop` ts)) (findIndices (groupCompare (word_match opt) g) grp) ++ 
        filterMatchingTokens opt f gs ts 
    where grp    = spanGroup grpLen ts
          grpLen = length g


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
groupCompareSemantic ts =  M.foldr (\xs r -> r && all (== head xs) xs) True m  
        where m =  M.mapWithKey (\k xs -> if k `elem` ["_", "$", "TOKEN_ANY", 
                                                       "TOKEN_NUMBER", "TOKEN_KEYWORD", 
                                                       "TOKEN_STRING", "TOKEN_CHAR",
                                                       "TOKEN_HEX", "TOKEN_OCT"]
                                              then []
                                              else xs ) $ M.fromListWith (++) (map snd ts)
        

tokensGroupCompare :: WordMatch -> [Pattern] -> [Cpp.Token] -> [(Bool, (String, [String]))]
tokensGroupCompare wordmatch l r 
    | length r >= length l = zipWith (tokensCompare wordmatch) l r
    | otherwise = [ (False, ("", [])) ]


tokensCompare :: WordMatch -> Pattern -> Cpp.Token -> (Bool, (String, [String]))

tokensCompare wm (Cpp.TokenIdentifier { Cpp.toString = l }) (Cpp.TokenIdentifier { Cpp.toString = r }) 
        | case l of 
            "TOKEN_ANY" -> True
            ('_':_)     -> True 
            ('$':_)     -> True
            _           -> False = (True, (l,[r]))
        | otherwise    =  if wm then (l == r, ("", []))
                                else (l `isInfixOf` r, ("", [])) 

tokensCompare _ (Cpp.TokenIdentifier { Cpp.toString = l }) (Cpp.TokenNumber { Cpp.toString = r }) 
        | l == "TOKEN_NUMBER"  = (True, (l, [r]))
        | l == "TOKEN_ANY"     = (True, (l, [r])) 
        | l == "TOKEN_OCT"     = (length r > 1 && '0' == head r && (isDigit . head . tail)  r, (l, [r]))
        | l == "TOKEN_HEX"     = ("0x" `isPrefixOf` r, (l, [r]))
        | otherwise            = (False, ("", []))

tokensCompare _ (Cpp.TokenIdentifier { Cpp.toString = l }) (Cpp.TokenKeyword { Cpp.toString = r }) 
        =  (l == "TOKEN_KEYWORD" || l == "TOKEN_ANY", (l,[r]))

tokensCompare _ (Cpp.TokenIdentifier { Cpp.toString = l }) (Cpp.TokenString { Cpp.toString = r }) 
        =  (l == "TOKEN_STRING"  || l == "TOKEN_ANY", (l,[r]))

tokensCompare _ (Cpp.TokenIdentifier { Cpp.toString = l }) (Cpp.TokenChar { Cpp.toString = r }) 
        =  (l == "TOKEN_CHAR" || l == "TOKEN_ANY", (l,[r]))

tokensCompare _ (Cpp.TokenIdentifier { Cpp.toString = l }) rt
        =  (l == "TOKEN_ANY", (l,[Cpp.toString rt]))

tokensCompare _ l r  = (Cpp.tokenCompare l r, ("", [])) 

 

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

module CGrep.Strategy.Cpp.Semantic (search) where

import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M

import CGrep.Filter 
import CGrep.Lang
import CGrep.Common
import CGrep.Output
import CGrep.Distance

import Data.Char
import Data.List
import Data.Function

import Options 
import Debug


import qualified CGrep.Strategy.Cpp.Token  as Cpp


search :: CgrepFunction
search opt ps f = do
    
    let filename = getFileName f 
    
    text <- getText f 

    -- transform text
    
    let text' = ignoreCase opt . expandMultiline opt . contextFilter (getLang opt filename) ((mkContextFilter opt) { getComment = False} ) $ text

    
    -- parse source code, get the Cpp.Token list...
    --
    
    let tokens = Cpp.tokenizer text'

    -- pre-process patterns
    --

    let patterns  = map (Cpp.tokenizer . contextFilter (Just Cpp) ((mkContextFilter opt) { getComment = False })) ps


    let patterns' = map (map mkWildCard) patterns >>= getWildCardSubsequence 

    -- get matching tokens ...
        
    let tokens' = sortBy (compare `on` Cpp.offset) $ nub (filterMatchingTokens opt filename patterns' tokens)   

    
    let matches = map (\t -> let n = fromIntegral (Cpp.offset t) in (n, Cpp.toString t)) tokens' :: [(Int, String)]


    putStrLevel1 (debug opt) $ "strategy  : running C/C++ semantic search on " ++ filename ++ "..."
    putStrLevel2 (debug opt) $ "patterns  : " ++ show patterns'
    putStrLevel2 (debug opt) $ "tokens    : " ++ show tokens'
    putStrLevel2 (debug opt) $ "matches   : " ++ show matches 
    putStrLevel3 (debug opt) $ "---\n" ++ C.unpack text' ++ "\n---"
    
    return $ mkOutput opt filename text matches
        

data WildCard = AnyCard               |
                KeyCard               |
                NumberCard            |
                OctCard               |
                HexCard               |
                StringCard            |
                CharCard              |
                IdentifierCard String |
                WildToken Cpp.Token         
                    deriving (Eq, Show, Ord)


wildCardMap :: M.Map String WildCard
wildCardMap = M.fromList 
            [
                ("ANY", AnyCard    ),
                ("KEY", KeyCard    ),
                ("OCT", OctCard    ),
                ("HEX", HexCard    ),
                ("NUM", NumberCard ),
                ("CHR", CharCard   ),
                ("STR", StringCard )
            ]


mkWildCard :: Cpp.Token -> WildCard
mkWildCard t@Cpp.TokenIdentifier { Cpp.toString = str } 
    | Just wc <-  M.lookup str wildCardMap =  wc 
    | ('$':_) <- str                       = IdentifierCard str
    | ('_':_) <- str                       = IdentifierCard str
    | otherwise                            = WildToken t
mkWildCard t = WildToken t


getWildCardSubsequence :: [WildCard] -> [[WildCard]]
getWildCardSubsequence wc = map (`filterCardIndicies` wc') idx
    where wc' = zip [0..] wc
          idx = subsequences $ findIndices (\w -> case w of 
                                                   IdentifierCard ('$':_) -> True  
                                                   _ -> False) wc


filterCardIndicies :: [Int] -> [(Int,WildCard)] -> [WildCard]
filterCardIndicies ns ps = map snd $ filter (\(n, _) -> n `notElem` ns) ps


filterMatchingTokens :: Options -> FilePath -> [[WildCard]] -> [Cpp.Token] -> [Cpp.Token]
filterMatchingTokens _ _ [] _ = []
filterMatchingTokens opt f (g:gs) ts = 
    concatMap (take grpLen . (`drop` ts)) (findIndices (groupCompare opt g) grp) ++ 
        filterMatchingTokens opt f gs ts 
    where grp    = spanGroup grpLen ts
          grpLen = length g


groupCompare :: Options -> [WildCard] -> [Cpp.Token] -> Bool
groupCompare opt l r = 
    groupCompareAll ts && groupCompareOptional ts
        where ts = tokensGroupCompare opt l r               


{-# INLINE groupCompareAll #-}

groupCompareAll :: [(Bool, (WildCard, [String]))] -> Bool
groupCompareAll = all fst 


{-# INLINE groupCompareOptional #-}

-- Note: pattern $ and _ match any token, whereas $1 $2 (_1 _2 etc.) match tokens
--       that must compare equal in the relative occurrences  
--       

groupCompareOptional :: [(Bool, (WildCard, [String]))] -> Bool
groupCompareOptional ts =  M.foldr (\xs r -> r && all (== head xs) xs) True m  
        where m =  M.mapWithKey (\k xs -> case k of 
                                                AnyCard             -> [] 
                                                NumberCard          -> [] 
                                                KeyCard             -> []
                                                StringCard          -> []
                                                CharCard            -> []
                                                HexCard             -> []
                                                OctCard             -> []
                                                IdentifierCard "_"  -> []
                                                IdentifierCard "$"  -> []
                                                _                   -> xs
                                  ) $ M.fromListWith (++) (map snd ts)
        

tokensGroupCompare :: Options -> [WildCard] -> [Cpp.Token] -> [(Bool, (WildCard, [String]))]
tokensGroupCompare opt ls rs 
    | length rs >= length ls = zipWith (tokensZip opt) ls rs
    | otherwise = [ (False, (AnyCard, [])) ]


tokensZip :: Options -> WildCard -> Cpp.Token -> (Bool, (WildCard, [String]))
tokensZip opt l r 
    |  wiildCardCompare opt l r = (True, (l, [Cpp.toString r]))
    |  otherwise                = (False,(AnyCard,[] ))


wiildCardCompare :: Options -> WildCard -> Cpp.Token -> Bool

wiildCardCompare _  (IdentifierCard _) (Cpp.TokenIdentifier {}) = True
wiildCardCompare _  AnyCard _                                   = True
wiildCardCompare _  KeyCard    (Cpp.TokenKeyword {}) = True
wiildCardCompare _  StringCard (Cpp.TokenString  {}) = True
wiildCardCompare _  CharCard   (Cpp.TokenChar    {}) = True
wiildCardCompare _  NumberCard (Cpp.TokenNumber  {}) = True
wiildCardCompare _  OctCard    (Cpp.TokenNumber  { Cpp.toString = r }) = case r of ('0':d: _) -> isDigit d; _ -> False
wiildCardCompare _  HexCard    (Cpp.TokenNumber  { Cpp.toString = r }) = case r of ('0':'x':_) -> True; _ -> False

wiildCardCompare opt (WildToken Cpp.TokenIdentifier {Cpp.toString = l}) (Cpp.TokenIdentifier {Cpp.toString = r}) 
    | edit_dist  opt =  l ~== r
    | word_match opt =  l == r
    | otherwise      =  l `isInfixOf` r

wiildCardCompare opt (WildToken Cpp.TokenString     {Cpp.toString = l}) (Cpp.TokenString     {Cpp.toString = r}) 
    | edit_dist  opt =  l ~== r
    | word_match opt =  l == r
    | otherwise      =  trim l `isInfixOf` r
        where trim = init . tail

wiildCardCompare _  (WildToken l) r = Cpp.tokenCompare l r 
wiildCardCompare _ _ _ = False


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


module CGrep.Strategy.BoyerMoore (search) where

import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Search as SC

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Arrow as A
import Data.List

import CGrep.Common
import CGrep.Output
import CGrep.Filter
import CGrep.Lang
import CGrep.Types

import qualified CGrep.Token as T

import Options
import Debug


search :: FilePath -> [Text8] -> ReaderT Options IO [Output]
search f patterns = do

    opt  <- ask
    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let [text''', _ , text', _] = scanr ($) text [ expandMultiline opt
                                                 , contextFilter (getFileLang opt filename) (mkContextFilter opt)
                                                 , ignoreCase opt
                                                 ]

    putStrLevel1 $ "strategy  : running Boyer-Moore search on " ++ filename ++ "..."

    runQuickSearch filename (quickSearch opt patterns text') $ do

        -- search for matching tokens

        let tokens  = map (A.second C.unpack) $ patterns >>= (\p -> map (\i -> (i,p)) (p `SC.nonOverlappingIndices` text'''))

        -- filter exact/partial matching tokens

            tokens' = if word_match opt || prefix_match opt || suffix_match opt
                        then filter (checkToken opt text''') tokens
                        else tokens

        -- print banners...

        putStrLevel2 $ "tokens    : " ++ show tokens
        putStrLevel2 $ "tokens'   : " ++ show tokens'
        putStrLevel3 $ "---\n" ++ C.unpack text''' ++ "\n---"

        mkOutput filename text text''' tokens'


checkToken :: Options -> Text8 -> (Offset, String) -> Bool
checkToken opt text (off, str)
     | word_match    opt = (off - off', str) `elem` ts
     | prefix_match  opt = any (\(o,s) -> str `isPrefixOf` s && o + off' == off) ts
     | suffix_match  opt = any (\(o,s) -> str `isSuffixOf` s && o + off' + (length s - length str) == off) ts
     where (text',off') = getLineByOffset off text
           ts           = T.tokenizer text'

checkToken _ _ (_,_) = undefined


splitLines :: Text8 -> [(Text8,Offset)]
splitLines xs = zip ls off
    where ls  = C.lines xs
          off = scanl (\o l -> 1 + o + C.length l) 0 ls


getLineByOffset :: Offset -> Text8 -> (Text8, Offset)
getLineByOffset off xs = last $ takeWhile (\(_,o) -> o <= off) sl
        where sl = splitLines xs


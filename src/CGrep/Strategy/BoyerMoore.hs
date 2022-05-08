--
-- Copyright (c) 2013-2022 Nicola Bonelli <nicola@pfq.io>
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

{-# LANGUAGE RecordWildCards #-}

module CGrep.Strategy.BoyerMoore (search) where

import qualified Data.ByteString.Char8 as C

import Control.Monad.Trans.Reader ( reader )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.List ( isSuffixOf, isPrefixOf, genericLength )

import CGrep.Common
    ( Text8,
      expandMultiline,
      getTargetContents,
      getTargetName,
      ignoreCase,
      runSearch,
      shallowSearch )
import CGrep.Output ( Output, mkOutput )
import CGrep.ContextFilter ( mkContextFilter )
import CGrep.LanguagesMap ( languageLookup, contextFilter )
import CGrep.Types ( Offset )

import qualified CGrep.Token as T

import Reader ( OptionIO )
import Options ( Options(word_match, prefix_match, suffix_match) )
import Verbose ( putStrLn1, putStrLn2, putStrLn3 )
import Util ( notNull )
import CGrep.Token (Token(..))


search :: FilePath -> [Text8] -> OptionIO [Output]
search f patterns = do

    opt  <- reader snd
    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let [text''', _ , _ , _] = scanr ($) text [ expandMultiline opt
                                              , contextFilter (languageLookup opt filename) (mkContextFilter opt)
                                              , ignoreCase opt
                                              ]

    -- make shallow search

    let shallow = shallowSearch patterns text'''

    -- search for matching tokens

    let tokens = concatMap (\(p, xs) -> (`Token` p) <$> xs ) $ zip patterns shallow

    -- filter exact/partial matching tokens

    let tokens' = if word_match opt || prefix_match opt || suffix_match opt
                    then filter (checkToken opt text''') tokens
                    else tokens

    putStrLn1 $ "strategy  : running Boyer-Moore search on " ++ filename ++ "..."

    runSearch opt filename (any notNull shallow) $ do

        -- print banners...

        putStrLn2 $ "tokens    : " ++ show tokens
        putStrLn2 $ "tokens'   : " ++ show tokens'
        putStrLn3 $ "---\n" ++ C.unpack text''' ++ "\n---"

        mkOutput filename text text''' tokens'


checkToken :: Options -> Text8 -> Token -> Bool
checkToken opt text Token{..}
     | word_match    opt = Token (tOffset - off') tStr `elem` ts
     | prefix_match  opt = any (\(Token o s) -> tStr `C.isPrefixOf` s && o + off' == tOffset) ts
     | suffix_match  opt = any (\(Token o s) -> tStr `C.isSuffixOf` s && o + off' + fromIntegral (C.length s - C.length tStr) == tOffset) ts
     | otherwise         = undefined
     where (text',off') = getLineByOffset tOffset text
           ts           = T.tokenizer text'


splitLines :: Text8 -> [(Text8, Offset)]
splitLines xs = zip ls off
    where ls  = C.lines xs
          off = fromIntegral<$> scanl (\o l -> 1 + o + C.length l) 0 ls
{-# INLINE splitLines #-}


getLineByOffset :: Offset -> Text8 -> (Text8, Offset)
getLineByOffset off xs = last $ takeWhile (\(_,o) -> o <= off) sl
        where sl = splitLines xs
{-# INLINE getLineByOffset #-}

--
-- Copyright (c) 2012-2013 Bonelli Nicola <bonelli@antifork.org>
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
 
{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeSynonymInstances #-} 

module CGrep.ParserTempl where
     
import Data.List
import Language.Haskell.TH


class DFABuilder a where
    m  :: a -> Q Exp         -- match
    l  :: a -> Q Exp         -- last match

instance DFABuilder Char where
    m cur = let c = mkName "c" in [| cur == $(global c) |]
    l _ = undefined


mkInvariant xs = (mkName "c", mkName "p", init xs, last xs)

instance DFABuilder String where

    m xs = let (c,p,pre,cur) = mkInvariant xs    
           in [| cur == $(global c) && pre `isSuffixOf` $(global p) |]

    l xs = let (c,p,pre,cur) = mkInvariant xs    
           in [| cur == $(global c) && not(pre `isSuffixOf` $(global p)) |]


{-# INLINE app1 #-}
app1 :: String -> Char -> String
app1 _ c = [c]


{-# INLINE app2 #-}
app2 :: String -> Char -> String
app2 (_:x:[]) c = [x,c]
app2 xs c = xs ++ [c]


{-# INLINE app3 #-}
app3 :: String -> Char -> String
app3 (_:x:y:[]) c = [x,y,c]
app3 xs c = xs ++ [c]


{-# INLINE app4 #-}
app4 :: String -> Char -> String
app4 (_:x:y:z:[]) c = [x,y,z,c]
app4 xs c = xs ++ [c]


{-# INLINE app5 #-}
app5 :: String -> Char -> String
app5 (_:x:y:w:z:[]) c = [x,y,w,z,c]
app5 xs c = xs ++ [c]


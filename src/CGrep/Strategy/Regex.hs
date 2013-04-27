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

{-# LANGUAGE FlexibleContexts #-} 

module CGrep.Strategy.Regex (cgrepRegex) where

import qualified Data.ByteString.Char8 as C

import CGrep.Function
import CGrep.Options 
import CGrep.StringLike
import CGrep.Output
import CGrep.Filter 
import CGrep.Lang

import Control.Monad (when)

import Text.Regex.Posix


cgrepRegex :: CgrepFunction
cgrepRegex opt ps f = do

    source <- if f == "" then slGetContents (ignore_case opt)
                         else slReadFile (ignore_case opt) f
    
    let filtered = if code opt || comment opt || literal opt
                     then filterContext (lookupLang f) ContextFilter { getCode = code opt, getComment = comment opt, getLiteral = literal opt } source
                     else source

    let content  = zip [1..] $ C.lines filtered 
    
    when (debug opt) $ do
        C.putStrLn filtered
        print opt 
        print content

    return $ concatMap (basicRegex opt f ps) content


basicRegex :: (StringLike a, RegexLike Regex a, RegexMaker Regex CompOption ExecOption a) => Options -> FilePath -> [a] -> (Int, a) -> [Output]
basicRegex opt f patterns (n, line) =
    if null patfilt
      then []
      else [Output f n line (map slToString patfilt)]
    where patfilt = filter (\p -> (line =~ p :: Bool) `xor` invert_match opt) patterns   


xor :: Bool -> Bool -> Bool
a `xor` b = a && not b || not a && b


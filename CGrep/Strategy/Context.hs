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

module CGrep.Strategy.Context (cgrepCppContext) where

import qualified Data.ByteString.Char8 as C
-- import qualified Data.ByteString.Lazy.Char8 as LC

import CGrep.Function
import CGrep.Options 
import CGrep.StringLike
import CGrep.Filter 

import Control.Monad (when)


cgrepCppContext :: CgrepFunction
cgrepCppContext opt ps f = do
    
    source <- if f == "" then slGetContents (ignore_case opt)
                         else slReadFile (ignore_case opt) f
    
    let filtered = filterContext ContextFilter { getCode = code opt, getComment = comment opt, getLiteral = literal opt } source
    let content  = zip [1..] $ C.lines filtered 

    when (debug opt) $ print content

    return $ concatMap (basicGrep opt f ps) content


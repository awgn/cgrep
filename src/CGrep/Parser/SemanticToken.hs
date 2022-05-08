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

module CGrep.Parser.SemanticToken (SemanticToken(..)) where
import Data.Int (Int64)


class (Show t, Ord t) => SemanticToken t where
    tkIsIdentifier :: t -> Bool
    tkIsString     :: t -> Bool
    tkIsChar       :: t -> Bool
    tkIsNumber     :: t -> Bool
    tkIsKeyword    :: t -> Bool
    tkToString     :: t -> String
    tkToOffset     :: t -> Int64
    tkToIdentif    :: String -> Int64 -> t
    tkEqual        :: t -> t -> Bool
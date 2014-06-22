--
-- Copyright (c) 2013 Bonelli Nicola <bonelli@antifork.org>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the StopNerds Public License as published by
-- the StopNerds Foundation; either version 1 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- StopNerds Public License for more details.
--
-- You should have received a copy of the StopNerds Public License
-- along with this program; if not, see <http://stopnerds.org/license/>.
--

module CGrep.Semantic.Token (SemanticToken(..)) where


class (Show t, Ord t) => SemanticToken t where
    tkIsIdentifier :: t -> Bool
    tkIsString     :: t -> Bool
    tkIsChar       :: t -> Bool
    tkIsNumber     :: t -> Bool
    tkIsKeyword    :: t -> Bool
    tkToString     :: t -> String
    tkEquivalent   :: t -> t -> Bool



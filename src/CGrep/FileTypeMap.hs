--
-- Copyright (c) 2013-2023 Nicola Bonelli <nicola@larthia.com>
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module CGrep.FileTypeMap (
    FileTypeMap (..),
    FileTypeInfo (..),
    FileTypeInfoMap (..),
    CharSet (..),
    WordType (..),
    IsCharSet (..),
    CharIdentifierF,
    (~~),
    reserved,
    types,
) where

import Language.Haskell.TH.Syntax (Lift)
import CGrep.FileType (FileSelector (..), FileType (..), ext, hdr, name)
import qualified Data.ByteString.Char8 as C
import CGrep.Boundary (Boundary (Boundary))
import CGrep.Parser.Char
import CGrep.FileKind

import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM

newtype FileTypeInfoMap = FileTypeInfoMap
    { unMapInfo :: Map.Map FileType FileTypeInfo
    } deriving stock (Eq, Show, Lift)

newtype FileTypeMap = FileTypeMap
    { unMap :: Map.Map FileSelector (FileType, FileKind)
    }

type CharIdentifierF = (Char -> Bool)

data WordType = Keyword | NativeType
    deriving stock (Eq, Ord, Show, Lift)

data FileTypeInfo = FileTypeInfo
    { ftSelector :: [FileSelector]
    , ftKind :: FileKind
    , ftChar :: [Boundary]
    , ftString :: [Boundary]
    , ftRawString :: [Boundary]
    , ftComment :: [Boundary]
    , ftIdentCharSet :: Maybe (CharSet, CharSet)
    , ftKeywords :: HM.HashMap C.ByteString WordType
    }
    deriving stock (Eq, Show, Lift)

data CharSet
    = None
    | Alpha
    | Alpha_
    | AlphaDollar_
    | AlphaNum
    | AlphaNum_
    | AlphaNum_'
    | AlphaNumDollar_
    | AlphaNumClojure_
    | AlphaDash_
    | AlphaNumDash_
    | Unicode_
    | UnicodeNum_
    | UnicodeDollar_
    | UnicodeNumDollar_
    | UnicodeNum_'
    | UnicodeXIDStart_
    | UnicodeNumXIDCont_
    | ClojureIdentStart
    | ClojureIdentCont
    | CSharpIdentStart
    | CSharpIdentCont
    | HtmlIdentStart
    | HtmlIdentCont
    | JavaIdentStart
    | JavaIdentCont
    | JuliaIdentStart
    | JuliaIdentCont
    | ListIdent
    | AgdaIdent

    deriving stock (Eq, Show, Lift)

class IsCharSet (cs :: CharSet) where
    isValidChar :: Char -> Bool

instance IsCharSet 'None where
    isValidChar _ = False
    {-# INLINE isValidChar #-}

instance IsCharSet 'Alpha where
    isValidChar = isAlpha
    {-# INLINE isValidChar #-}

instance IsCharSet 'Alpha_ where
    isValidChar = isAlpha_
    {-# INLINE isValidChar #-}

instance IsCharSet 'AlphaNum where
    isValidChar = isAlphaNum
    {-# INLINE isValidChar #-}

instance IsCharSet 'AlphaNum_ where
    isValidChar = isAlphaNum_
    {-# INLINE isValidChar #-}

instance IsCharSet 'AlphaNum_' where
    isValidChar = isAlphaNum_'
    {-# INLINE isValidChar #-}

instance IsCharSet 'AlphaDollar_ where
    isValidChar = isAlphaDollar_
    {-# INLINE isValidChar #-}

instance IsCharSet 'AlphaNumDollar_ where
    isValidChar = isAlphaNumDollar_
    {-# INLINE isValidChar #-}

instance IsCharSet 'ClojureIdentStart where
    isValidChar = isClojureIdentStart
    {-# INLINE isValidChar #-}

instance IsCharSet 'ClojureIdentCont where
    isValidChar = isClojureIdentCont
    {-# INLINE isValidChar #-}

instance IsCharSet 'CSharpIdentStart where
    isValidChar = isCSharpIdentStart
    {-# INLINE isValidChar #-}

instance IsCharSet 'CSharpIdentCont where
    isValidChar = isCSharpIdentCont
    {-# INLINE isValidChar #-}

instance IsCharSet 'AlphaDash_ where
    isValidChar = isAlphaDash_
    {-# INLINE isValidChar #-}

instance IsCharSet 'AlphaNumDash_ where
    isValidChar = isAlphaNumDash_
    {-# INLINE isValidChar #-}

instance IsCharSet 'HtmlIdentStart where
    isValidChar = isHtmlIdentStart
    {-# INLINE isValidChar #-}

instance IsCharSet 'HtmlIdentCont where
    isValidChar = isHtmlIdentCont
    {-# INLINE isValidChar #-}

instance IsCharSet 'JavaIdentStart where
    isValidChar = isJavaIdentStart
    {-# INLINE isValidChar #-}

instance IsCharSet 'JavaIdentCont where
    isValidChar = isJavaIdentCont
    {-# INLINE isValidChar #-}

instance IsCharSet 'JuliaIdentStart where
    isValidChar = isJuliaIdentStart
    {-# INLINE isValidChar #-}

instance IsCharSet 'JuliaIdentCont where
    isValidChar = isJuliaIdentCont
    {-# INLINE isValidChar #-}

instance IsCharSet 'ListIdent where
    isValidChar = isLispIdent
    {-# INLINE isValidChar #-}

instance IsCharSet 'Unicode_ where
    isValidChar = isUnicode_
    {-# INLINE isValidChar #-}

instance IsCharSet 'UnicodeNum_ where
    isValidChar = isUnicodeNum_
    {-# INLINE isValidChar #-}

instance IsCharSet 'UnicodeNum_' where
    isValidChar = isUnicodeNum_'
    {-# INLINE isValidChar #-}

instance IsCharSet 'UnicodeDollar_ where
    isValidChar = isUnicodeDollar_
    {-# INLINE isValidChar #-}

instance IsCharSet 'UnicodeNumDollar_ where
    isValidChar = isUnicodeNumDollar_
    {-# INLINE isValidChar #-}

instance IsCharSet 'UnicodeXIDStart_ where
    isValidChar = isUnicodeXIDStart_
    {-# INLINE isValidChar #-}

instance IsCharSet 'UnicodeNumXIDCont_ where
    isValidChar = isUnicodeNumXIDCont_
    {-# INLINE isValidChar #-}

instance IsCharSet 'AgdaIdent where
    isValidChar = isAgdaIdent
    {-# INLINE isValidChar #-}

(~~) :: C.ByteString -> C.ByteString -> Boundary
(~~) = Boundary
{-# INLINE (~~) #-}

reserved :: [C.ByteString] -> HM.HashMap C.ByteString WordType
reserved = HM.fromList . map (,Keyword)
{-# INLINE reserved #-}

types :: [C.ByteString] -> HM.HashMap C.ByteString WordType
types = HM.fromList . map (,NativeType)
{-# INLINE types #-}

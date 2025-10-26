{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE KindSignatures #-}
--
-- Copyright (c) 2013-2025 Nicola Bonelli <nicola@larthia.com>
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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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

import CGrep.Boundary (Boundary (..))
import CGrep.FileKind
import CGrep.FileType (FileSelector (..), FileType (..))
import CGrep.Parser.Char
import Language.Haskell.TH.Syntax (Lift)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as T

newtype FileTypeInfoMap = FileTypeInfoMap
    { unMapInfo :: Map.Map FileType FileTypeInfo
    }
    deriving stock (Eq, Show, Lift)

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
    , ftKeywords :: HM.HashMap T.Text WordType
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

(~~) :: T.Text -> T.Text -> Boundary
b ~~ e = Boundary{bBegin = b, bBeginLen = T.length b, bEnd = e, bEndLen = T.length e}
{-# INLINE (~~) #-}

reserved :: [T.Text] -> HM.HashMap T.Text WordType
reserved = HM.fromList . map (,Keyword)
{-# INLINE reserved #-}

types :: [T.Text] -> HM.HashMap T.Text WordType
types = HM.fromList . map (,NativeType)
{-# INLINE types #-}

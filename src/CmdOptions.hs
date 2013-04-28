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

module CmdOptions where

import System.Console.CmdArgs
import System.Directory
import System.FilePath ((</>))

import Control.Monad

import Data.List (isPrefixOf)
import Data.Char
import Data.Maybe

import CGrep.Lang

import Config
import Options


options = cmdArgsMode $ Options 
          {
                file  = ""  &= typ "FILE"  &= help "read PATTERNs from file" &= groupname "Pattern",
                word  = False              &= help "force word matching",
                regex = False              &= help "regex matching" &= explicit &= name "G" &=name "regex",
                ignore_case = False        &= help "ignore case distinctions",

                code = False               &= help "grep in source code"     &= explicit &= name "c" &= name "code" &= groupname "Context filters",
                comment = False            &= help "grep in comments"        &= explicit &= name "m" &= name "comment",
                literal = False            &= help "grep in string literals" &= explicit &= name "l" &= name "literal",

                identifier = False         &= help "identifiers" &= explicit &= name "identifier" &= groupname "C/C++ language",
                keyword = False            &= help "keywords" &= explicit &= name "keyword",
                directive = False          &= help "preproc directives" &= explicit &= name "directive",
                header = False             &= help "headers name" &= explicit &= name "header",
                number = False             &= help "literal numbers" &= explicit &= name "number",
                string = False             &= help "literal strings" &= explicit &= name "string",
                char = False               &= help "literal chars" &= explicit &= name "char",
                oper = False               &= help "operators" &= explicit &= name "oper",
                
                no_filename = False        &= help "suppress the file name prefix on output"  &= explicit &= name "h" &= name "no-filename" &= groupname "Output control",
                no_linenumber= False       &= help "suppress the line number on output lines" &= explicit &= name "N" &= name "no-line-umber",
                lang = []                  &= help "specify languages to grep for. ie: Cpp, +Haskell, -Makefile",
                lang_map = False           &= help "show language -> ext map",

                jobs   = 1                 &= help "number of jobs" &= groupname "General",
                multiline = False          &= help "enable multi-line matching",
                recursive = False          &= help "enable recursive search",
                invert_match = False       &= help "select non-matching lines" &= explicit &= name "invert-match", 
                debug = False              &= help "debug mode",
                others = []                &= args

          } &= summary ("Cgrep " ++ version ++ ". Usage: cgrep [OPTION] [PATTERN] files...") &= program "cgrep"


data  CgrepOptions = CgrepOptions
                    {
                        language :: [Lang],
                        pruneDir :: [String]
                    } deriving (Show,Read)


-- parse cmdArg language list:
--

parseLangList :: [String] -> ([Lang], [Lang], [Lang])
parseLangList  = foldl run ([],[],[]) 
                    where run :: ([Lang], [Lang], [Lang]) -> String -> ([Lang], [Lang], [Lang])
                          run (l1, l2, l3) l
                            | ('+':xs) <- l = (l1, (prettyRead xs "Lang") : l2, l3)
                            | ('-':xs) <- l = (l1, l2, (prettyRead xs "Lang") : l3)
                            | otherwise     = ((prettyRead l  "Lang") : l1, l2, l3)  


-- parse CgrepOptions from ~/.cgreprc, or /etc/cgreprc 
--


getCgrepOptions :: IO CgrepOptions
getCgrepOptions = do
    home <- getHomeDirectory
    conf <- liftM msum $ forM [ home </> "." ++ cgreprc, "/etc" </> cgreprc ] $ \f ->
                doesFileExist f >>= \b -> 
                    return $ if b then Just f else Nothing
    if isJust conf then readFile (fromJust conf) >>= \xs -> return (read (dropComments xs) :: CgrepOptions) 
                   else return $ CgrepOptions [] []
    where dropComments :: String -> String
          dropComments = unlines . filter (not . isPrefixOf "#" . dropWhile isSpace) . lines


prettyRead :: Read a => String -> String -> a
prettyRead xs ys = case value of
                        Just v -> v
                        _      -> error $ "parse error: '" ++ xs ++ "' bad " ++ ys ++ "!"
                   where value = readMaybe xs
              

readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads



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


module CGrep.Parser  where

import Data.List

data Context = Code | Comment | Literal
                deriving (Eq, Show)


data ContextFilter = ContextFilter { getCode    :: Bool,
                                     getComment :: Bool,
                                     getLiteral :: Bool 
                     } deriving (Eq, Show)

data FiltState = FiltState 
                 {
                    cstate  :: ContextState,
                    cfilter :: ContextFilter,
                    pchar   :: String
                 } deriving (Eq, Show)



data ContextState = StateCode       | 
                    StateComment    | 
                    StateComment2   | 
                    StateComment3   | 
                    StateLiteral    |
                    StateLiteral2
                        deriving (Eq, Show)

type FilterFunction = (String,Char) -> FiltState -> (Context, FiltState)

-- Parser...
--

parser1 :: Char -> FilterFunction
parser1 c0 (p,c) filtstate@(FiltState StateCode _ _) 
    | c == c0   = (Code, filtstate { cstate = StateComment,  pchar = app1 p c })
    | c == '"'  = (Code, filtstate { cstate = StateLiteral,  pchar = app1 p c })
    | c == '\'' = (Code, filtstate { cstate = StateLiteral2, pchar = app1 p c }) 
    | otherwise = (Code, filtstate { pchar = app1 p c } )
parser1 _ (_,c) filtstate@(FiltState StateComment _ _)
    | c == '\n' = (Comment, filtstate { cstate = StateCode, pchar = app1 [] c })
    | otherwise = (Comment, filtstate { pchar = app1 [] c })
parser1 _ (p,c) filtstate@(FiltState StateLiteral _ _)
    | p /= "\\" && c == '"'  = (Code,    filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Literal, filtstate { pchar = app1 p c }) 
parser1 _ (p,c) filtstate@(FiltState StateLiteral2 _ _)
    | p /= "\\" && c == '\'' = (Code, filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Literal, filtstate { pchar = app1 p c})


likeShell :: FilterFunction
likeShell = parser1 '#'

likeErlang :: FilterFunction
likeErlang = parser1 '%'

likeLatex :: FilterFunction
likeLatex = parser1 '%'

likeVim :: FilterFunction
likeVim = parser1 '"'


likeCpp :: FilterFunction
likeCpp (p,c) filtstate@(FiltState StateCode _ _) 
    | p == "/"  && c == '/'  = (Code, filtstate { cstate = StateComment2,  pchar = app1 p c })
    | p == "/"  && c == '*'  = (Code, filtstate { cstate = StateComment,   pchar = app1 p c })
    |              c == '"'  = (Code, filtstate { cstate = StateLiteral,   pchar = app1 p c })
    |              c == '\'' = (Code, filtstate { cstate = StateLiteral2,  pchar = app1 p c }) 
    | p == "\\" && c == '\\' = (Code, filtstate { pchar = app1 p ' ' })
    | otherwise = (Code, filtstate { pchar = app1 p c } )
likeCpp (_,c) filtstate@(FiltState StateComment2 _ _)
    | c == '\n' = (Comment, filtstate { cstate = StateCode, pchar = app1 [] c })
    | otherwise = (Comment, filtstate { pchar = app1 [] c })
likeCpp (p,c) filtstate@(FiltState StateComment _ _)
    | p == "*" && c == '/'  = (Comment, filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Comment, filtstate { pchar = app1 p c })
likeCpp (p,c) filtstate@(FiltState StateLiteral _ _)
    | p /= "\\" && c == '"'  = (Code,    filtstate { cstate = StateCode, pchar = app1 p c })
    | p == "\\" && c == '\\' = (Literal, filtstate { pchar = app1 p ' '})
    | otherwise = (Literal, filtstate { pchar = app1 p c }) 
likeCpp (p,c) filtstate@(FiltState StateLiteral2 _ _)
    | p /= "\\" && c == '\'' = (Code, filtstate { cstate = StateCode, pchar = app1 p c })
    | p == "\\" && c == '\\' = (Literal, filtstate { pchar = app1 p ' ' })
    | otherwise = (Literal, filtstate { pchar = app1 p c})


likeHaskell :: FilterFunction
likeHaskell (p,c) filtstate@(FiltState StateCode _ _) 
    | p == "-"  && c == '-'  = (Code, filtstate { cstate = StateComment2,  pchar = app1 p c })
    | p == "{"  && c == '-'  = (Code, filtstate { cstate = StateComment,   pchar = app1 p c })
    |              c == '"'  = (Code, filtstate { cstate = StateLiteral,   pchar = app1 p c })
    |              c == '\'' = (Code, filtstate { cstate = StateLiteral2,  pchar = app1 p c }) 
    | otherwise = (Code, filtstate { pchar = app1 p c } )
likeHaskell (_,c) filtstate@(FiltState StateComment2 _ _)
    | c == '\n' = (Comment, filtstate { cstate = StateCode, pchar = app1 [] c })
    | otherwise = (Comment, filtstate { pchar = app1 [] c })
likeHaskell (p,c) filtstate@(FiltState StateComment _ _)
    | p == "-" && c == '}'  = (Comment, filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Comment, filtstate { pchar = app1 p c })
likeHaskell (p,c) filtstate@(FiltState StateLiteral _ _)
    | p /= "\\" && c == '"'  = (Code,    filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Literal, filtstate { pchar = app1 p c }) 
likeHaskell (p,c) filtstate@(FiltState StateLiteral2 _ _)
    | p /= "\\" && c == '\'' = (Code, filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Literal, filtstate { pchar = app1 p c})


likePerl :: FilterFunction
likePerl (p,c) filtstate@(FiltState StateCode _ _) 
    |               c == '#'  = (Code, filtstate { cstate = StateComment,  pchar = app3 p c })
    | p == "=po" && c == 'd'  = (Code, filtstate { cstate = StateComment2,  pchar = app3 p c })
    |               c == '"'  = (Code, filtstate { cstate = StateLiteral,  pchar = app3 p c })
    |               c == '\'' = (Code, filtstate { cstate = StateLiteral2, pchar = app3 p c }) 
    | otherwise = (Code, filtstate { pchar = app3 p c } )
likePerl (_,c) filtstate@(FiltState StateComment _ _)
    | c == '\n' = (Comment, filtstate { cstate = StateCode, pchar = app3 [] c })
    | otherwise = (Comment, filtstate { pchar = app3 [] c })
likePerl (p,c) filtstate@(FiltState StateComment2 _ _)
    | p == "=cu" && c == 't' = (Comment, filtstate { cstate = StateCode, pchar = app3 [] c })
    | otherwise = (Comment, filtstate { pchar = app3 p c })
likePerl (p,c) filtstate@(FiltState StateLiteral _ _)
    | not("\\" `isSuffixOf` p) && c == '"'  = (Code,    filtstate { cstate = StateCode, pchar = app3 p c })
    | otherwise = (Literal, filtstate { pchar = app3 p c }) 
likePerl (p,c) filtstate@(FiltState StateLiteral2 _ _)
    | not("\\" `isSuffixOf` p) && c == '\'' = (Code, filtstate { cstate = StateCode, pchar = app3 p c })
    | otherwise = (Literal, filtstate { pchar = app3 p c})


likeCSS :: FilterFunction
likeCSS (p,c) filtstate@(FiltState StateCode _ _) 
    | p == "/"  && c == '*'  = (Code, filtstate { cstate = StateComment,   pchar = app1 p c })
    |              c == '"'  = (Code, filtstate { cstate = StateLiteral,   pchar = app1 p c })
    |              c == '\'' = (Code, filtstate { cstate = StateLiteral2,  pchar = app1 p c }) 
    | otherwise = (Code, filtstate { pchar = app1 p c } )
likeCSS (p,c) filtstate@(FiltState StateComment _ _)
    | p == "*" && c == '/'  = (Comment, filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Comment, filtstate { pchar = app1 p c })
likeCSS (p,c) filtstate@(FiltState StateLiteral _ _)
    | p /= "\\" && c == '"'  = (Code,    filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Literal, filtstate { pchar = app1 p c }) 
likeCSS (p,c) filtstate@(FiltState StateLiteral2 _ _)
    | p /= "\\" && c == '\'' = (Code, filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Literal, filtstate { pchar = app1 p c})


likeFsharp :: FilterFunction
likeFsharp (p,c) filtstate@(FiltState StateCode _ _) 
    | p == "/"  && c == '/'  = (Code, filtstate { cstate = StateComment2,  pchar = app1 p c })
    | p == "("  && c == '*'  = (Code, filtstate { cstate = StateComment,   pchar = app1 p c })
    |              c == '"'  = (Code, filtstate { cstate = StateLiteral,   pchar = app1 p c })
    |              c == '\'' = (Code, filtstate { cstate = StateLiteral2,  pchar = app1 p c }) 
    | p == "\\" && c == '\\' = (Code, filtstate { pchar = app1 p ' ' })
    | otherwise = (Code, filtstate { pchar = app1 p c } )
likeFsharp (_,c) filtstate@(FiltState StateComment2 _ _)
    | c == '\n' = (Comment, filtstate { cstate = StateCode, pchar = app1 [] c })
    | otherwise = (Comment, filtstate { pchar = app1 [] c })
likeFsharp (p,c) filtstate@(FiltState StateComment _ _)
    | p == "*" && c == ')'  = (Comment, filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Comment, filtstate { pchar = app1 p c })
likeFsharp (p,c) filtstate@(FiltState StateLiteral _ _)
    | p /= "\\" && c == '"'  = (Code,    filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Literal, filtstate { pchar = app1 p c }) 
likeFsharp (p,c) filtstate@(FiltState StateLiteral2 _ _)
    | p /= "\\" && c == '\'' = (Code, filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Literal, filtstate { pchar = app1 p c})


likeOCaml :: FilterFunction
likeOCaml (p,c) filtstate@(FiltState StateCode _ _) 
    | p == "("  && c == '*'  = (Code, filtstate { cstate = StateComment,   pchar = app1 p c })
    |              c == '"'  = (Code, filtstate { cstate = StateLiteral,   pchar = app1 p c })
    |              c == '\'' = (Code, filtstate { cstate = StateLiteral2,  pchar = app1 p c }) 
    | p == "\\" && c == '\\' = (Code, filtstate { pchar = app1 p ' ' })
    | otherwise = (Code, filtstate { pchar = app1 p c } )
likeOCaml (p,c) filtstate@(FiltState StateComment _ _)
    | p == "*" && c == ')'  = (Comment, filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Comment, filtstate { pchar = app1 p c })
likeOCaml (p,c) filtstate@(FiltState StateLiteral _ _)
    | p /= "\\" && c == '"'  = (Code,    filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Literal, filtstate { pchar = app1 p c }) 
likeOCaml (p,c) filtstate@(FiltState StateLiteral2 _ _)
    | p /= "\\" && c == '\'' = (Code, filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Literal, filtstate { pchar = app1 p c})


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


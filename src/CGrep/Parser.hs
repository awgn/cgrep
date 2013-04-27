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
 
{-# LANGUAGE TemplateHaskell #-} 


module CGrep.Parser  where

import CGrep.ParserTempl

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
                    StateLiteral    |
                    StateLiteral2
                        deriving (Eq, Show)

type FilterFunction = (String,Char) -> FiltState -> (Context, FiltState)

-- Parser...
--

parser1 :: Char -> FilterFunction
parser1 c0 (p,c) fs@(FiltState StateCode _ _) 
    | c == c0           = (Code, fs { cstate = StateComment,  pchar = [] })
    | $(m '"')          = (Code, fs { cstate = StateLiteral,  pchar = [] })
    | $(m '\'')         = (Code, fs { cstate = StateLiteral2, pchar = [] }) 
    | otherwise         = (Code, fs { pchar = app1 p c } )

parser1 _ (_,c) fs@(FiltState StateComment _ _)
    | $(m '\n')         = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app1 [] c })

parser1 _ (p,c) fs@(FiltState StateLiteral _ _)
    | $(l "\\\"")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app1 p c }) 

parser1 _ (p,c) fs@(FiltState StateLiteral2 _ _)
    | $(l "\\'")        = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app1 p c})


likeShell :: FilterFunction
likeShell = parser1 '#'

likeErlang :: FilterFunction
likeErlang = parser1 '%'

likeLatex :: FilterFunction
likeLatex = parser1 '%'

likeVim :: FilterFunction
likeVim = parser1 '"'


-- C/C++ ------------------

likeCpp :: FilterFunction
likeCpp (p,c) fs@(FiltState StateCode _ _) 
    | $(m "/*")         = (Code, fs { cstate = StateComment,  pchar = [] })
    | $(m "//")         = (Code, fs { cstate = StateComment2, pchar = [] })
    | $(m '"')          = (Code, fs { cstate = StateLiteral,  pchar = [] })
    | $(m '\'')         = (Code, fs { cstate = StateLiteral2, pchar = [] }) 
    | otherwise         = (Code, fs { pchar = app1 p c } )

likeCpp (_,c) fs@(FiltState StateComment2 _ _)
    | $(m '\n')         = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app1 [] c })

likeCpp (p,c) fs@(FiltState StateComment _ _)
    | $(m "*/")         = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app1 p c })

likeCpp (p,c) fs@(FiltState StateLiteral _ _)
    | $(l "\\\"")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app1 p c }) 

likeCpp (p,c) fs@(FiltState StateLiteral2 _ _)
    | $(l "\\\'")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app1 p c})


-- Haskell ------------------

likeHaskell :: FilterFunction
likeHaskell (p,c) fs@(FiltState StateCode _ _) 
    | $(m "{-")         = (Code, fs { cstate = StateComment,  pchar = [] })
    | $(m "--")         = (Code, fs { cstate = StateComment2, pchar = [] })
    | $(m '"')          = (Code, fs { cstate = StateLiteral,  pchar = [] })
    | $(m '\'')         = (Code, fs { cstate = StateLiteral2, pchar = [] }) 
    | otherwise         = (Code, fs { pchar = app1 p c } )

likeHaskell (_,c) fs@(FiltState StateComment2 _ _)
    | $(m '\n')         = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app1 [] c })

likeHaskell (p,c) fs@(FiltState StateComment _ _)
    | $(m "-}")         = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app1 p c })

likeHaskell (p,c) fs@(FiltState StateLiteral _ _)
    | $(l "\\\"")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app1 p c }) 

likeHaskell (p,c) fs@(FiltState StateLiteral2 _ _)
    | $(l "\\\'")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app1 p c})


-- Perl ------------------
    
likePerl :: FilterFunction
likePerl (p,c) fs@(FiltState StateCode _ _) 
    | $(m '#')          = (Code, fs { cstate = StateComment,  pchar = [] })
    | $(m "=pod")       = (Code, fs { cstate = StateComment2, pchar = [] })
    | $(m '"')          = (Code, fs { cstate = StateLiteral,  pchar = [] })
    | $(m '\'')         = (Code, fs { cstate = StateLiteral2, pchar = [] }) 
    | otherwise         = (Code, fs { pchar = app3 p c } )

likePerl (_,c) fs@(FiltState StateComment _ _)
    | $(m '\n')         = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app3 [] c })

likePerl (p,c) fs@(FiltState StateComment2 _ _)
    | $(m "=cut")       = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app3 p c })

likePerl (p,c) fs@(FiltState StateLiteral _ _)
    | $(l "\\\"")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app3 p c }) 

likePerl (p,c) fs@(FiltState StateLiteral2 _ _)
    | $(l "\\\'")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app3 p c})


-- CSS ------------------

likeCSS :: FilterFunction
likeCSS (p,c) fs@(FiltState StateCode _ _) 
    | $(m "/*")         = (Code, fs { cstate = StateComment,   pchar = [] })
    | $(m '"')          = (Code, fs { cstate = StateLiteral,   pchar = [] })
    | $(m '\'')         = (Code, fs { cstate = StateLiteral2,  pchar = [] }) 
    | otherwise         = (Code, fs { pchar = app1 p c } )

likeCSS (p,c) fs@(FiltState StateComment _ _)
    | $(m "*/")         = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app1 p c })

likeCSS (p,c) fs@(FiltState StateLiteral _ _)
    | $(l "\\\"")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app1 p c }) 

likeCSS (p,c) fs@(FiltState StateLiteral2 _ _)
    | $(l "\\\'")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app1 p c})


-- F# ------------------

likeFsharp :: FilterFunction
likeFsharp (p,c) fs@(FiltState StateCode _ _) 
    | $(m "//")         = (Code, fs { cstate = StateComment2,  pchar = [] })
    | $(m "(*")         = (Code, fs { cstate = StateComment,   pchar = [] })
    | $(m '"')          = (Code, fs { cstate = StateLiteral,   pchar = [] })
    | $(m '\'')         = (Code, fs { cstate = StateLiteral2,  pchar = [] }) 
    | otherwise         = (Code, fs { pchar = app1 p c } )

likeFsharp (_,c) fs@(FiltState StateComment2 _ _)
    | $(m '\n')         = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app1 [] c })

likeFsharp (p,c) fs@(FiltState StateComment _ _)
    | $(m "*)")         = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app1 p c })

likeFsharp (p,c) fs@(FiltState StateLiteral _ _)
    | $(l "\\\"")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app1 p c }) 

likeFsharp (p,c) fs@(FiltState StateLiteral2 _ _)
    | $(l "\\\'")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app1 p c})


-- OCaml ------------------

likeOCaml :: FilterFunction
likeOCaml (p,c) fs@(FiltState StateCode _ _) 
    | $(m "(*")         = (Code, fs { cstate = StateComment,   pchar = [] })
    | $(m '"')          = (Code, fs { cstate = StateLiteral,   pchar = [] })
    | $(m '\'')         = (Code, fs { cstate = StateLiteral2,  pchar = [] }) 
    | otherwise         = (Code, fs { pchar = app1 p c } )

likeOCaml (p,c) fs@(FiltState StateComment _ _)
    | $(m "*)")         = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app1 p c })

likeOCaml (p,c) fs@(FiltState StateLiteral _ _)
    | $(l "\\\"")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app1 p c }) 

likeOCaml (p,c) fs@(FiltState StateLiteral2 _ _)
    | $(l "\\\'")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise = (Literal, fs { pchar = app1 p c})


-- PHP ------------------
    
likePHP :: FilterFunction
likePHP (p,c) fs@(FiltState StateCode _ _) 
    | $(m "/*")         = (Code, fs { cstate = StateComment,   pchar = [] })
    | $(m "//")         = (Code, fs { cstate = StateComment2,  pchar = [] })
    | $(m '#')          = (Code, fs { cstate = StateComment2,  pchar = [] })
    | $(m '"')          = (Code, fs { cstate = StateLiteral,   pchar = [] })
    | $(m '\'')         = (Code, fs { cstate = StateLiteral2,  pchar = [] }) 
    | otherwise         = (Code, fs { pchar = app1 p c } )

likePHP (_,c) fs@(FiltState StateComment2 _ _)
    | $(m '\n')         = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app1 [] c })

likePHP (p,c) fs@(FiltState StateComment _ _)
    | $(m "*/")         = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise = (Comment, fs { pchar = app1 p c })

likePHP (p,c) fs@(FiltState StateLiteral _ _)
    | $(l "\\\"")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app1 p c }) 

likePHP (p,c) fs@(FiltState StateLiteral2 _ _)
    | $(l "\\'")        = (Code, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app1 p c})


-- Python ------------------
    
likePython :: FilterFunction
likePython (p,c) fs@(FiltState StateCode _ _) 
    | $(m '#')          = (Code, fs { cstate = StateComment,  pchar = [] })
    | $(m '"')          = (Code, fs { cstate = StateLiteral,  pchar = [] })
    | $(m '\'')         = (Code, fs { cstate = StateLiteral2, pchar = [] }) 
    | otherwise         = (Code, fs { pchar = app1 p c } )

likePython (_,c) fs@(FiltState StateComment _ _)
    | $(m '\n')         = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app1 [] c })

likePython (p,c) fs@(FiltState StateLiteral _ _)
    | $(l "\\\"")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise = (Literal, fs { pchar = app1 p c }) 

likePython (p,c) fs@(FiltState StateLiteral2 _ _)
    | $(l "\\'")        = (Code, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app1 p c})


-- Ruby ------------------
    
likeRuby :: FilterFunction
likeRuby (p,c) fs@(FiltState StateCode _ _) 
    | $(m '#')          = (Code, fs { cstate = StateComment,  pchar = [] })
    | $(m "=begin")     = (Code, fs { cstate = StateComment2, pchar = [] })
    | $(m '"')          = (Code, fs { cstate = StateLiteral,  pchar = [] })
    | $(m '\'')         = (Code, fs { cstate = StateLiteral2, pchar = [] }) 
    | otherwise         = (Code, fs { pchar = app5 p c } )

likeRuby (_,c) fs@(FiltState StateComment _ _)
    | $(m '\n')         = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app5 [] c })

likeRuby (p,c) fs@(FiltState StateComment2 _ _)
    | $(m "=end")       = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar = app5 p c })

likeRuby (p,c) fs@(FiltState StateLiteral _ _)
    | $(l "\\\"")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app5 p c }) 

likeRuby (p,c) fs@(FiltState StateLiteral2 _ _)
    | $(l "\\'")        = (Code, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar = app5 p c})


-- Html ------------------
    
likeHtml :: FilterFunction
likeHtml (p,c) fs@(FiltState StateCode _ _) 
    | $(m "<!--" )      = (Code, fs { cstate = StateComment,  pchar = [] })
    | $(m '"')          = (Code, fs { cstate = StateLiteral,  pchar = [] })
    | $(m '\'')         = (Code, fs { cstate = StateLiteral2, pchar = [] }) 
    | otherwise         = (Code, fs { pchar  = app3 p c } )

likeHtml (p,c) fs@(FiltState StateComment _ _)
    | $(m "-->")        = (Comment, fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Comment, fs { pchar  = app3 p c })

likeHtml (p,c) fs@(FiltState StateLiteral _ _)
    | $(l "\\\"")       = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar  = app3 p c }) 

likeHtml (p,c) fs@(FiltState StateLiteral2 _ _)
    | $(l "\\'")        = (Code,    fs { cstate = StateCode, pchar = [] })
    | otherwise         = (Literal, fs { pchar  = app3 p c })



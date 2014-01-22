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

module CGrep.Template where

import CGrep.Context

import Data.List
import Language.Haskell.TH


splitLast :: String -> (String, Char)
splitLast xs = (init xs, last xs)


parser1 :: (String, String) -> Q Exp
parser1 (c0,c1) = [| \(p,c) fs -> case fs of 

    FiltState StateCode _ _ ->          case () of 
                                        _  | (p,c) `matchS` (c0a,c0b) -> (Code, fs { pchars = [], cstate = StateComment  })
                                           | c == '"'                 -> (Code, fs { pchars = [], cstate = StateLiteral  })
                                           | c == '\''                -> (Code, fs { pchars = [], cstate = StateLiteral2 }) 
                                           | otherwise                -> (Code, fs { pchars = $(global app) p c } )

    FiltState StateComment _ _  ->      case () of 
                                         _ | (p,c) `matchS` (c1a,c1b) -> (Comment, fs { pchars = [], cstate = StateCode })
                                           | otherwise                -> (Comment, fs { pchars = $(global app) p c })
     
    FiltState StateComment2 _ _  ->     undefined 

    FiltState StateComment3 _ _  ->     undefined 

    FiltState StateLiteral _ _  ->      case () of 
                                         _  | (p,c) `matchC` '"'         -> (Code,    fs { pchars = [], cstate = StateCode })
                                            | (p,c) `matchS` ("\\",'\\') -> (Literal, fs { pchars = $(global app) p '|' })
                                            | otherwise                  -> (Literal, fs { pchars = $(global app) p c }) 

    FiltState StateLiteral2 _ _  ->     case () of 
                                         _  | (p,c) `matchC` '\''        -> (Code,    fs { pchars = [], cstate = StateCode })
                                            | (p,c) `matchS` ("\\",'\\') -> (Literal, fs { pchars = $(global app) p '|' })
                                            | otherwise                  -> (Literal, fs { pchars = $(global app) p c })
    |]
        where len = max (length c0) (length c1)
              app = mkName ("app" ++ show (len-1))
              (c0a,c0b) = splitLast c0
              (c1a,c1b) = splitLast c1


parser2 :: (String, String) -> (String, String) -> Q Exp
parser2 (c0,c1) (c2,c3) = [| \(p,c) fs -> case fs of 

    FiltState StateCode _ _ ->          case () of
                                        _   | (p,c) `matchS` (c0a,c0b) -> (Code, fs { pchars = [], cstate = StateComment  })
                                            | (p,c) `matchS` (c2a,c2b) -> (Code, fs { pchars = [], cstate = StateComment2 })
                                            | c == '"'                 -> (Code, fs { pchars = [], cstate = StateLiteral  })
                                            | c == '\''                -> (Code, fs { pchars = [], cstate = StateLiteral2 }) 
                                            | otherwise                -> (Code, fs { pchars  = $(global app) p c } )

    FiltState StateComment _ _ ->       case () of
                                        _   | (p,c) `matchS` (c1a,c1b) -> (Comment, fs { pchars = [], cstate = StateCode  })
                                            | otherwise                -> (Comment, fs { pchars = $(global app) p c })
    
    FiltState StateComment2 _ _ ->      case () of
                                        _   | (p,c) `matchS` (c3a,c3b) -> (Comment, fs { pchars = [], cstate = StateCode  })
                                            | otherwise                -> (Comment, fs { pchars = $(global app) p c })

    FiltState StateComment3 _ _  ->     undefined 
    
    FiltState StateLiteral _ _ ->       case () of
                                        _   | (p,c) `matchC` '"'         -> (Code,    fs { pchars = [], cstate = StateCode  })
                                            | (p,c) `matchS` ("\\",'\\') -> (Literal, fs { pchars = $(global app) p '|' })
                                            | otherwise                  -> (Literal, fs { pchars = $(global app) p  c  }) 
    
    FiltState StateLiteral2 _ _ ->      case () of
                                        _   | (p,c) `matchC` '\''        -> (Code,    fs { pchars = [], cstate = StateCode  })
                                            | (p,c) `matchS` ("\\",'\\') -> (Literal, fs { pchars = $(global app) p '|' })
                                            | otherwise                  -> (Literal, fs { pchars = $(global app) p c})
    |]
        where len = max (length c0) (length c1)
              app = mkName ("app" ++ show (len-1))
              (c0a,c0b) = splitLast c0
              (c1a,c1b) = splitLast c1
              (c2a,c2b) = splitLast c2
              (c3a,c3b) = splitLast c3
       

parser3 :: (String, String) -> (String, String) -> (String,String) -> Q Exp
parser3 (c0,c1) (c2,c3) (c4,c5) = [| \(p,c) fs -> case fs of 

    FiltState StateCode _ _ ->          case () of
                                        _   | (p,c) `matchS` (c0a,c0b) -> (Code, fs { pchars = [], cstate = StateComment  })
                                            | (p,c) `matchS` (c2a,c2b) -> (Code, fs { pchars = [], cstate = StateComment2 })
                                            | (p,c) `matchS` (c4a,c4b) -> (Code, fs { pchars = [], cstate = StateComment3 })
                                            | c == '"'                 -> (Code, fs { pchars = [], cstate = StateLiteral  })
                                            | c == '\''                -> (Code, fs { pchars = [], cstate = StateLiteral2 }) 
                                            | otherwise                -> (Code, fs { pchars  = $(global app) p c } )

    FiltState StateComment _ _ ->       case () of
                                        _   | (p,c) `matchS` (c1a,c1b) -> (Comment, fs { pchars = [], cstate = StateCode  })
                                            | otherwise                -> (Comment, fs { pchars = $(global app) p c })
    
    FiltState StateComment2 _ _ ->      case () of
                                        _   | (p,c) `matchS` (c3a,c3b) -> (Comment, fs { pchars = [], cstate = StateCode  })
                                            | otherwise                -> (Comment, fs { pchars = $(global app) p c })

    FiltState StateComment3 _ _ ->      case () of
                                        _   | (p,c) `matchS` (c5a,c5b) -> (Comment, fs { pchars = [], cstate = StateCode  })
                                            | otherwise                -> (Comment, fs { pchars = $(global app) p c })
    
    FiltState StateLiteral _ _ ->       case () of
                                        _   | (p,c) `matchC` '"'         -> (Code,    fs { pchars = [], cstate = StateCode })
                                            | (p,c) `matchS` ("\\",'\\') -> (Literal, fs { pchars = $(global app) p '|' })
                                            | otherwise                  -> (Literal, fs { pchars = $(global app) p c }) 
    
    FiltState StateLiteral2 _ _ ->      case () of
                                        _   | (p,c) `matchC` '\''        -> (Code,    fs { pchars = [], cstate = StateCode })
                                            | (p,c) `matchS` ("\\",'\\') -> (Literal, fs { pchars = $(global app) p '|' })
                                            | otherwise                  -> (Literal, fs { pchars = $(global app) p c})
    |]
        where len = max (length c0) (length c1)
              app = mkName ("app" ++ show (len-1))
              (c0a,c0b) = splitLast c0
              (c1a,c1b) = splitLast c1
              (c2a,c2b) = splitLast c2
              (c3a,c3b) = splitLast c3
              (c4a,c4b) = splitLast c4
              (c5a,c5b) = splitLast c5


{-# INLINE matchS #-}

matchS :: (String,Char) -> (String,Char) -> Bool 
matchS (p,c) (pre,cur) = cur == c && pre `isSuffixOf` p  


{-# INLINE matchC #-}

matchC :: (String,Char) -> Char -> Bool 
matchC (p,c) cur = cur == c && not ("\\" `isSuffixOf` p) 


{-# INLINE app0 #-}

app0 :: String -> Char -> String
app0 _ c = [c]


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


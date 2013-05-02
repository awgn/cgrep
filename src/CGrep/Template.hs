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

import CGrep.FilterData

import Data.List
import Language.Haskell.TH


parser1 :: (String, String) -> Q Exp
parser1 (c0,c1) = [| \(p,c) fs -> case fs of 

    FiltState StateCode _ _ ->          case () of 
                                        _  | matches p c c0 -> (Code, fs { pchars = [], cstate = StateComment  })
                                           | c == '"'       -> (Code, fs { pchars = [], cstate = StateLiteral  })
                                           | c == '\''      -> (Code, fs { pchars = [], cstate = StateLiteral2 }) 
                                           | otherwise      -> (Code, fs { pchars = $(global app) p c } )

    FiltState StateComment _ _  ->      case () of 
                                         _ | matches p c c1 -> (Comment, fs { pchars = [], cstate = StateCode })
                                           | otherwise      -> (Comment, fs { pchars = $(global app) p c })
     
    FiltState StateComment2 _ _  ->     undefined 

    FiltState StateComment3 _ _  ->     undefined 

    FiltState StateLiteral _ _  ->      case () of 
                                         _  | c == '"' && p /= "\\"  -> (Code,    fs { pchars = [], cstate = StateCode })
                                            | otherwise              -> (Literal, fs { pchars = $(global app) p c }) 

    FiltState StateLiteral2 _ _  ->     case () of 
                                         _  | c == '\'' && p /= "\\" -> (Code,    fs { pchars = [], cstate = StateCode })
                                            | otherwise              -> (Literal, fs { pchars = $(global app) p c })
    |]
        where len = max (length c0) (length c1)
              app = mkName ("app" ++ show (len-1))


parser2 :: (String, String) -> (String, String) -> Q Exp
parser2 (c0,c1) (c2,c3) = [| \(p,c) fs -> case fs of 

    FiltState StateCode _ _ ->          case () of
                                        _   | matches p c c0        -> (Code, fs { pchars = [], cstate = StateComment  })
                                            | matches p c c2        -> (Code, fs { pchars = [], cstate = StateComment2 })
                                            | c == '"'              -> (Code, fs { pchars = [], cstate = StateLiteral  })
                                            | c == '\''             -> (Code, fs { pchars = [], cstate = StateLiteral2 }) 
                                            | otherwise             -> (Code, fs { pchars  = $(global app) p c } )

    FiltState StateComment _ _ ->       case () of
                                        _   | matches p c c1        -> (Comment, fs { pchars = [], cstate = StateCode  })
                                            | otherwise             -> (Comment, fs { pchars = $(global app) p c })
    
    FiltState StateComment2 _ _ ->      case () of
                                        _   | matches p c c3        -> (Comment, fs { pchars = [], cstate = StateCode  })
                                            | otherwise             -> (Comment, fs { pchars = $(global app) p c })

    FiltState StateComment3 _ _  ->     undefined 
    
    FiltState StateLiteral _ _ ->       case () of
                                        _   | mlast p c "\\\""      -> (Code,    fs { pchars = [], cstate = StateCode  })
                                            | otherwise             -> (Literal, fs { pchars = $(global app) p c }) 
    
    FiltState StateLiteral2 _ _ ->      case () of
                                        _   | mlast p c "\\\'"      -> (Code,    fs { pchars = [], cstate = StateCode  })
                                            | otherwise             -> (Literal, fs { pchars = $(global app) p c})
    |]
        where len = max (length c0) (length c1)
              app = mkName ("app" ++ show (len-1))


parser3 :: (String, String) -> (String, String) -> (String,String) -> Q Exp
parser3 (c0,c1) (c2,c3) (c4,c5) = [| \(p,c) fs -> case fs of 

    FiltState StateCode _ _ ->          case () of
                                        _   | matches p c c0        -> (Code, fs { pchars = [], cstate = StateComment  })
                                            | matches p c c2        -> (Code, fs { pchars = [], cstate = StateComment2 })
                                            | matches p c c4        -> (Code, fs { pchars = [], cstate = StateComment3 })
                                            | c == '"'              -> (Code, fs { pchars = [], cstate = StateLiteral  })
                                            | c == '\''             -> (Code, fs { pchars = [], cstate = StateLiteral2 }) 
                                            | otherwise             -> (Code, fs { pchars  = $(global app) p c } )

    FiltState StateComment _ _ ->       case () of
                                        _   | matches p c c1        -> (Comment, fs { pchars = [], cstate = StateCode  })
                                            | otherwise             -> (Comment, fs { pchars = $(global app) p c })
    
    FiltState StateComment2 _ _ ->      case () of
                                        _   | matches p c c3        -> (Comment, fs { pchars = [], cstate = StateCode  })
                                            | otherwise             -> (Comment, fs { pchars = $(global app) p c })

    FiltState StateComment3 _ _ ->      case () of
                                        _   | matches p c c5        -> (Comment, fs { pchars = [], cstate = StateCode  })
                                            | otherwise             -> (Comment, fs { pchars = $(global app) p c })
    
    FiltState StateLiteral _ _ ->       case () of
                                        _   | mlast p c "\\\""      -> (Code,    fs { pchars = [], cstate = StateCode })
                                            | otherwise             -> (Literal, fs { pchars = $(global app) p c }) 
    
    FiltState StateLiteral2 _ _ ->      case () of
                                        _   | mlast p c "\\\'"      -> (Code,    fs { pchars = [], cstate = StateCode })
                                            | otherwise             -> (Literal, fs { pchars = $(global app) p c})
    |]
        where len = max (length c0) (length c1)
              app = mkName ("app" ++ show (len-1))


{-# INLINE matches #-}

matches :: String -> Char -> String -> Bool 
matches p c xs = cur == c && pre `isSuffixOf` p  
                   where  cur = last xs
                          pre = init xs


{-# INLINE mlast #-}

mlast :: String -> Char -> String -> Bool 
mlast p c xs = cur == c && not (pre `isSuffixOf` p) 
                    where  cur = last xs
                           pre = init xs

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


{-# LANGUAGE TemplateHaskell #-} 

module CGrep.ContextParser  where


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
                    pchar   :: [Char]
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


likeShell :: FilterFunction
likeShell (p,c) filtstate@(FiltState StateCode _ _) 
    |              c == '#'  = (Code, filtstate { cstate = StateComment,  pchar = app1 p c })
    |              c == '"'  = (Code, filtstate { cstate = StateLiteral,  pchar = app1 p c })
    |              c == '\'' = (Code, filtstate { cstate = StateLiteral2, pchar = app1 p c }) 
    | otherwise = (Code, filtstate { pchar = app1 p c } )
likeShell (_,c) filtstate@(FiltState StateComment _ _)
    | c == '\n' = (Comment, filtstate { cstate = StateCode, pchar = app1 [] c })
    | otherwise = (Comment, filtstate { pchar = app1 [] c })
likeShell (p,c) filtstate@(FiltState StateLiteral _ _)
    | p /= "\\" && c == '"'  = (Code,    filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Literal, filtstate { pchar = app1 p c }) 
likeShell (p,c) filtstate@(FiltState StateLiteral2 _ _)
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


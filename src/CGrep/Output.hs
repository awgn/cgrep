-- Copyright (c) 2013-2019 Nicola Bonelli <nicola@pfq.io>
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

{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

module CGrep.Output ( Output(..)
                    , mkOutput
                    , putOutputHeader
                    , putOutputFooter
                    , putOutput
                    , showFileName
                    , showFile
                    , showBold) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Codec.Binary.UTF8.String as UC

import Text.Show.Unicode ( ushow )
import System.Console.ANSI
    ( setSGRCode,
      ConsoleIntensity(BoldIntensity),
      SGR(SetConsoleIntensity) )

#ifdef ENABLE_HINT
import Language.Haskell.Interpreter
#endif

import Control.Monad.Trans.Reader ( ask, reader )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Maybe ( fromJust, isJust )
import Data.List
    ( foldl', sortBy, groupBy, intercalate, isPrefixOf, nub, sort, genericLength )
import Data.Function ( on )

import CGrep.Types ( Text8, LineOffset, Offset2d, Offset )
import CGrep.Token ( Line, Token )

import Options
    ( Options(Options, invert_match, filename_only, json, xml,
              no_filename, count, format, no_numbers, no_column, show_match,
              color, no_color) )

import Util ( notNull )
import Config ( Config(configColorFile, configColorMatch) )
import Reader ( OptionIO )
import Safe ( atDef )
import Data.Int ( Int64 )


data Output = Output
    { outFilePath :: !FilePath
    , outLineNumb :: {-# UNPACK #-} !Int64
    , outLine     :: {-# UNPACK #-} !Text8
    , outTokens   :: ![Token]
    }
    deriving (Show)


getOffsetsLines :: Text8 -> [Int64]
getOffsetsLines txt =
    let l = C.length txt
        ret = filter (<(l-1)) $ C.elemIndices '\n' txt
    in  fromIntegral <$> ret
{-# INLINE getOffsetsLines #-}

getOffset2d :: [LineOffset] -> Offset -> Offset2d
getOffset2d idx off =
  let prc = filter (< off) idx
      (!len_prc, !last_prc) = foldl' (\(len,_ ) cur -> let !l1 = len+1 in  (l1, cur)) (0, off) prc
  in (# len_prc, off - last_prc - 1 #)
{-# INLINE getOffset2d #-}


mkOutput :: FilePath -> Text8 -> Text8 -> [Token] -> OptionIO [Output]
mkOutput f text multi ts = do
    invert <- reader (invert_match . snd)
    return $ if invert then map (\(n, xs) -> Output f n (ls !! fromIntegral (n-1)) xs) . invertLines (length ls) $ mkLines multi ts
                       else map (\(n, xs) -> Output f n (ls !! fromIntegral (n-1)) xs) $ mkLines multi ts
    where ls = C.lines text
{-# INLINE mkOutput #-}


mkLines :: Text8 -> [Token] -> [Line]
mkLines _ [] = []
mkLines text ts = map mergeGroup $ groupBy ((==) `on` fst) $
    sortBy (compare `on` fst) $ map (\t -> let (# r, c #) = getOffset2d ols (fst t) in (1 + r, [(c, snd t)])) ts
    where mergeGroup ls = (fst $ head ls, foldl (\l m -> l ++ snd m) [] ls)
          ols = getOffsetsLines text


invertLines :: Int -> [Line] -> [Line]
invertLines n xs =  filter (\(i,_) ->  i `notElem` idx ) $ take n [ (i, []) | i <- [1..]]
    where idx = map fst xs
{-# INLINE invertLines #-}


putOutputHeader :: OptionIO ()
putOutputHeader = do
   (_,opt) <- ask
   if  | xml  opt  -> liftIO $ putStrLn "<?xml version=\"1.0\"?>" >> putStrLn "<cgrep>"
       | otherwise -> return ()


putOutputFooter :: OptionIO ()
putOutputFooter = do
    (_,opt) <- ask
    if | xml  opt  -> liftIO $ putStrLn "</cgrep>"
       | otherwise -> return ()


putOutput :: [Output] -> OptionIO [String]
putOutput out = do
    (_,opt) <- ask
    if  | isJust $ format opt -> mapM formatOutput out
        | filename_only opt   -> filenameOutput out
        | json opt            -> jsonOutput out
        | xml opt             -> xmlOutput  out
#ifdef ENABLE_HINT
        | isJust $ hint opt   -> hintOputput out
#endif
        | otherwise           -> defaultOutput out


defaultOutput :: [Output] -> OptionIO [String]
defaultOutput xs = do
    (conf,opt) <- ask
    case () of
        _ |  Options{ no_filename = False, no_numbers = False , count = False } <- opt
                -> return $ map (\out -> concat $ [showFile conf, showSep ":", showLineCol, showSep " ", showTokens, showLine conf] <*> [opt] <*> [out]) xs

          |  Options{ no_filename = False, no_numbers = True  , count = False } <- opt
                -> return $ map (\out -> concat $ [showFile conf, showSep ":", showTokens,  showLine conf] <*> [opt] <*> [out] ) xs

          |  Options{ no_filename = True , no_numbers = False , count = False } <- opt
                -> return $ map (\out -> concat $ [showLineCol, showSep " ",  showTokens, showLine conf] <*> [opt] <*> [out] ) xs

          |  Options{ no_filename = True , no_numbers = True  , count = False } <- opt
                -> return $ map (\out -> concat $ [showTokens, showLine conf] <*> [opt] <*>  [out]) xs

          |  Options{ no_filename = False, count = True } <- opt -> do let gs = groupBy (\(Output f1 _ _ _) (Output f2 _ _ _) -> f1 == f2) xs
                                                                       return $ map (\ys@(y:_) -> showFile conf opt y ++ ":" ++ show (length ys)) gs

          |  Options{ count = True } <- opt -> do let gs = groupBy (\(Output f1 _ _ _) (Output f2 _ _ _) -> f1 == f2) xs
                                                  return $ map (show . length) gs

jsonOutput :: [Output] -> OptionIO [String]
jsonOutput outs = return $
    [" { \"file\": " ++ show fname ++ ", \"matches\": ["] ++
    [ intercalate "," (foldl mkMatch [] outs) ] ++
    ["] }"]
        where fname | (Output f _ _ _) <- head outs = f
              mkToken (n, xs) = "{ \"col\": " ++ show n ++ ", \"token\": " ++ show xs ++ " }"
              mkMatch xs (Output _ n l ts) = xs ++ [ "{ \"row\": " ++ show n ++ ", \"tokens\": [" ++ intercalate "," (map mkToken ts) ++ "], \"line\":" ++ show l ++ "}" ]


filenameOutput :: [Output] -> OptionIO [String]
filenameOutput outs = return $ nub $ map (\(Output fname _ _ _) -> fname) outs
{-# INLINE filenameOutput #-}


xmlOutput :: [Output] -> OptionIO [String]
xmlOutput outs = return $
    ["<file name=" ++ show fname ++ ">" ] ++
    ["<matches>" ] ++
    [foldl mkMatch "" outs] ++
    ["</matches>"] ++
    ["</file>"]
        where fname | (Output f _ _ _) <- head outs = f
              mkToken (n, xs) = "<token col=\"" ++ show n ++ "\" >" ++ xs ++ "</token>"
              mkMatch xs (Output _ n l ts) = xs ++  "<match line=" ++ show l ++ " row=\"" ++ show n ++ "\">" ++
                                                    unwords (map mkToken ts) ++
                                                    "</match>"

formatOutput :: Output -> OptionIO String
formatOutput out = do
    (conf,opt) <- ask
    return $ replace (fromJust $ format opt)
        [
            ("#f", showFile conf opt out),
            ("#n", showLineCol opt out),
            ("#l", showLine conf opt out),
            ("#t", ushow ts'),
            ("##", unwords ts'),
            ("#,", intercalate "," ts'),
            ("#;", intercalate ";" ts'),
            ("#0", atDef "" ts' 0),
            ("#1", atDef "" ts' 1),
            ("#2", atDef "" ts' 2),
            ("#3", atDef "" ts' 3),
            ("#4", atDef "" ts' 4),
            ("#5", atDef "" ts' 5),
            ("#6", atDef "" ts' 6),
            ("#7", atDef "" ts' 7),
            ("#8", atDef "" ts' 8),
            ("#9", atDef "" ts' 9)
        ]
    where ts' = map snd (outTokens out)


replace :: String -> [(String, String)] -> String
replace ys@(x:xs) pats =
  let pats' = filter ((`isPrefixOf` ys) . fst) pats  in
  if null pats' then x : replace xs pats
                else let new = head pats' in snd new ++ replace (drop (length(fst new) - 1) xs) pats
replace [] _ = []


#ifdef ENABLE_HINT
hintOputput :: [Output] -> OptionT IO [String]
hintOputput outs = do
    (_,opt) <- ask
    let cmds = map mkCmd outs
    out <- runInterpreter $ setImports ["Prelude", "Data.List"] >> mapM (`interpret` (as :: String)) cmds
    return $ either ((:[]) . show) id out
        where mkCmd out@(Output f n l ts) = "let a # b = a !! b " ++
                                             "; file   = " ++ show (showFile opt out) ++
                                             "; row    = " ++ show n ++
                                             "; line   = " ++ show (showLine conf opt ts l) ++
                                             "; tokens = " ++ ushow (map snd ts) ++ " in " ++
                                            (fromJust $ hint opt)
#endif

bold, resetTerm :: String
bold      = setSGRCode [SetConsoleIntensity BoldIntensity]
resetTerm = setSGRCode []
{-# INLINE bold #-}
{-# INLINE resetTerm #-}

type ColorString = String


showSep  :: String -> Options -> Output -> String
showSep xs _ _ = xs
{-# INLINE showSep #-}


showFile :: Config -> Options -> Output -> String
showFile conf opt = showFileName conf opt . outFilePath
{-# INLINE showFile #-}


showLineCol :: Options -> Output -> String
showLineCol Options{no_numbers = True } _ = ""
showLineCol Options{no_numbers = False, no_column = True  } (Output _ n _ _)  = show n
showLineCol Options{no_numbers = False, no_column = False } (Output _ n _ []) = show n
showLineCol Options{no_numbers = False, no_column = False } (Output _ n _ ts) = show n ++ ":" ++ show ((+1) . fst . head $ ts)
{-# INLINE showLineCol #-}


showTokens :: Options -> Output -> String
showTokens Options { show_match = st } out
    | st        = ushow (map snd (outTokens out))
    | otherwise = ""
{-# INLINE showTokens #-}


showLine :: Config -> Options -> Output -> String
showLine conf Options { color = c, no_color = c' } out
    | c && not c'= hilightLine conf (sortBy (flip compare `on` (length . snd )) (outTokens out)) line
    | otherwise  = line
    where line = UC.decode $ B.unpack $ outLine out
{-# INLINE showLine #-}


showFileName :: Config -> Options -> String -> String
showFileName conf opt = showColoredAs opt $ setSGRCode (configColorFile conf)
{-# INLINE showFileName #-}


showBold :: Options -> String -> String
showBold opt = showColoredAs opt bold
{-# INLINE showBold #-}


showColoredAs :: Options -> ColorString -> String -> String
showColoredAs Options { color = c, no_color = c'} colorCode str
    | c && not c'= colorCode ++ str ++ resetTerm
    | otherwise  = str
{-# INLINE showColoredAs #-}


hilightLine :: Config -> [Token] -> String -> String
hilightLine conf ts =  hilightLine' (hilightIndicies ts, 0, 0)
    where hilightLine' :: ([(Int64, Int64)], Int64, Int) -> String -> String
          hilightLine'  _ [] = []
          hilightLine' (ns, n, bs) s@(x:_) = (case () of
                                                  _ | check && bs' == 0 -> if fst stack > 0 then colorMatch ++ [x] ++ resetTerm
                                                                                            else x : resetTerm
                                                    | check && bs' > 0 -> colorMatch ++ [x]
                                                    | otherwise -> next
                                             ) ++ hilightLine' (ns, n + nn, bs') rest
            where stack = foldr (\(a, b) (c, d) -> (c + fromEnum (a == n), d + fromEnum (b == n))) (0, 0) ns
                  check = fst stack > 0 || snd stack > 0
                  colorMatch = setSGRCode (configColorMatch conf)
                  bs' = bs + fst stack - snd stack
                  plain = nub . sort $ foldr (\(a, b) acc -> a : b : acc) [] ns
                  nn | check = 1
                     | null plain' = genericLength s
                     | otherwise = head plain' - n
                         where plain' = dropWhile (<=n) plain
                  (next, rest) = splitAt (fromIntegral nn) s


hilightIndicies :: [Token] -> [(Int64, Int64)]
hilightIndicies = foldr (\t a -> let b = fst t in (fromIntegral b, b + genericLength (snd t) - 1) : a) [] . filter (notNull . snd)
{-# INLINE hilightIndicies #-}

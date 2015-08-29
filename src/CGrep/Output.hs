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

module CGrep.Output (Output(),
                     mkOutput,
                     putPrettyHeader,
                     putPrettyFooter,
                     prettyOutput,
                     showFile) where

import qualified Data.ByteString.Char8 as C
import System.Console.ANSI

#ifdef ENABLE_HINT
import Language.Haskell.Interpreter
#endif

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Data.Maybe
import Data.List
import Data.List.Split
import Data.Function

import CGrep.Types
import CGrep.Token

import Safe
import Options


data Output = Output FilePath Int Text8 [Token]
    deriving (Show)


getOffsetsLines :: Text8 -> [Int]
getOffsetsLines = C.elemIndices '\n'


getOffset2d :: [OffsetLine] -> Offset -> Offset2d
getOffset2d idx off = let prc =  fst $ partition (< off) idx in
        case prc of
          [] -> (0, off)
          _  -> (length prc, off - last prc - 1)


mkOutput :: Options -> FilePath -> Text8 -> Text8 -> [Token] -> [Output]
mkOutput Options { invert_match = invert } f text multi ts
    | invert    = map (\(n, xs) -> Output f n (ls !! (n-1)) xs) . invertMatchLines (length ls) $ mkMatchLines multi ts
    | otherwise = map (\(n, xs) -> Output f n (ls !! (n-1)) xs) $ mkMatchLines multi ts
        where ls = C.lines text


mkMatchLines :: Text8 -> [Token] -> [MatchLine]
mkMatchLines _ [] = []
mkMatchLines text ts = map mergeGroup $ groupBy ((==) `on` fst) $
    sortBy (compare `on` fst) $ map (\t -> let (r,c) = getOffset2d ols (fst t) in (1 + r, [(c, snd t)])) ts
    where mergeGroup ls = (fst $ head ls, foldl (\l m -> l ++ snd m) [] ls)
          ols = getOffsetsLines text


invertMatchLines :: Int -> [MatchLine] -> [MatchLine]
invertMatchLines n xs =  filter (\(i,_) ->  i `notElem` idx ) $ take n [ (i, []) | i <- [1..]]
    where idx = map fst xs


putPrettyHeader :: ReaderT Options IO ()
putPrettyHeader = do
    opt <- ask
    case () of
      _  | json opt  -> liftIO $ putStrLn "["
         | xml  opt  -> liftIO $ putStrLn "<?xml version=\"1.0\"?>" >> putStrLn "<cgrep>"
         | otherwise -> return ()


putPrettyFooter :: ReaderT Options IO ()
putPrettyFooter = do
    opt <- ask
    case () of
      _  | json opt  -> liftIO $ putStrLn "]"
         | xml  opt  -> liftIO $ putStrLn "</cgrep>"
         | otherwise -> return ()


prettyOutput :: [Output] -> ReaderT Options IO [String]
prettyOutput out = do
    opt <- ask
    case () of
        _ | isJust $ format opt -> return $ map (formatOutput opt) out
          | json opt            -> return $ jsonOutput opt out
          | xml opt             -> return $ xmlOutput opt out
#ifdef ENABLE_HINT
          | isJust $ hint opt   -> hintOputput opt out
#endif
          | otherwise           -> return $ defaultOutput opt out


defaultOutput :: Options -> [Output] -> [String]

defaultOutput opt@Options{ no_filename = False, no_linenumber = False , count = False } xs =
    map (\(Output f n l ts) -> showFile opt f ++ ":" ++ show n ++ ":" ++ showTokens opt ts ++ showLine opt ts l) xs
defaultOutput opt@Options{ no_filename = False, no_linenumber = True  , count = False } xs =
    map (\(Output f _ l ts) -> showFile opt f ++ ":" ++ showTokens opt ts ++ showLine opt ts l) xs
defaultOutput opt@Options{ no_filename = True , no_linenumber = False , count = False } xs =
    map (\(Output _ n l ts) -> show n ++ ":" ++ showTokens opt ts ++ showLine opt ts l) xs
defaultOutput opt@Options{ no_filename = True , no_linenumber = True  , count = False } xs =
    map (\(Output _ _ l ts) -> showTokens opt ts ++ showLine opt ts l) xs
defaultOutput opt@Options{ count = True } xs =
    let gs = groupBy (\(Output f1 _ _ _) (Output f2 _ _ _) -> f1 == f2) xs
    in map (\ys@(y:_) -> showFile opt (outputFilename y) ++ ":" ++ show (length ys)) gs
    where outputFilename (Output f _ _ _) = f


jsonOutput :: Options -> [Output] -> [String]
jsonOutput _ outs =
    [" { \"file\": " ++ show fname ++ ", \"matches\": ["] ++
    [ intercalate "," (foldl mkMatch [] outs) ] ++
    ["] }"]
        where fname | (Output f _ _ _) <- head outs = f
              mkToken (n, xs) = "{ \"col\": " ++ show n ++ ", \"token\": " ++ show xs ++ " }"
              mkMatch xs (Output _ n l ts) = xs ++ [ "{ \"row\": " ++ show n ++ ", \"tokens\": [" ++ intercalate "," (map mkToken ts) ++ "], \"line\":" ++ show l ++ "}" ]


xmlOutput :: Options -> [Output] -> [String]
xmlOutput _ outs =
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

formatOutput :: Options -> Output -> String
formatOutput opt (Output f n l ts) =
    foldl trans (fromJust $ format opt)
        [
            ("#f", showFile opt f),
            ("#n", show n),
            ("#l", showLine opt ts l),
            ("#t", show ts'),
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
    where trans str (old, new) = replace old new str
          ts' = map snd ts


replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old


#ifdef ENABLE_HINT
hintOputput :: Options -> [Output] -> IO [String]
hintOputput opt outs = do
    let cmds = map mkCmd outs
    out <- runInterpreter $ setImports ["Prelude", "Data.List"] >> mapM (`interpret` (as :: String)) cmds
    return $ either ((:[]) . show) id out
        where mkCmd (Output f n l ts) = "let a # b = a !! b " ++
                                          "; file   = " ++ show (showFile opt f) ++
                                          "; row    = " ++ show n ++
                                          "; line   = " ++ show (showLine opt ts l) ++
                                          "; tokens = " ++ show (map snd ts) ++ " in " ++
                                         (fromJust $ hint opt)
#endif

blue, bold, resetTerm :: String

blue      = setSGRCode [SetColor Foreground Vivid Blue]
bold      = setSGRCode [SetConsoleIntensity BoldIntensity]
resetTerm = setSGRCode []


showTokens :: Options -> [Token] -> String
showTokens Options { show_match = st } xs
    | st        = show (map snd xs)
    | otherwise = ""


showFile :: Options -> String -> String
showFile Options { color = c } f
    | c         = bold ++ blue ++ f ++ resetTerm
    | otherwise = f


showLine :: Options -> [Token] -> Line8 -> String
showLine Options { color = c } ts l
    | c         = hilightLine (sortBy (flip compare `on` (length . snd )) ts) (C.unpack l)
    | otherwise = C.unpack l


hilightLine :: [Token] -> String -> String
hilightLine ts =  hilightLine' (hilightIndicies ts, 0)
    where hilightLine' :: ([Int],Int) -> String -> String
          hilightLine'  _ [] = []
          hilightLine' (ns,n) (x:xs) = (if n `elem` ns then bold ++ [x] ++ resetTerm
                                                       else [x]) ++ hilightLine' (ns, n+1) xs


hilightIndicies :: [Token] -> [Int]
hilightIndicies = concatMap (\(o, s) -> take (length s) [o..])


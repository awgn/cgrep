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

module CGrep.Output (
    OutputMatch (..),
    mkOutputMatches,
    putOutputMatches,
    runSearch,
    showFileName,
    showBold,
) where

import qualified Data.Vector.Unboxed as UV

import Data.Vector.Unboxed ((!))

import System.Console.ANSI (
    ConsoleIntensity (BoldIntensity),
    SGR (SetConsoleIntensity),
    setSGRCode,
 )

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ask, reader)

import Data.Function (on)
import Data.List (
    foldl',
    genericLength,
    groupBy,
    intersperse,
    isPrefixOf,
    nub,
    sort,
    sortBy,
 )

import CGrep.Parser.Chunk (Chunk (..), MatchLine (..))

import Config (Config (configColorFile, configColorMatch))
import Data.Int (Int64)
import qualified Data.Vector.Fusion.Util as VU (Box (..))
import Data.Word (Word8)
import Reader (Env (..), ReaderIO)

import System.OsPath (OsPath, decodeUtf)
import System.OsString.Internal.Types

import CGrep.Line (LineIndex, getLineByOffset', getLineOffsets, lookupLineAndPosition, totalLines)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Vector.Generic as GV
import Debug.Trace (traceShowId)
import Options (
    Options (
        Options,
        color,
        count,
        filename_only,
        invert_match,
        json,
        no_color,
        no_column,
        no_filename,
        no_numbers,
        no_shallow,
        show_match
    ),
 )
import qualified OsPath as OS

data OutputMatch = OutputMatch
    { outFilePath :: OsPath
    , outLineNumb :: {-# UNPACK #-} !Int
    , outLine :: {-# UNPACK #-} !T.Text
    , outChunks :: ![Chunk]
    }
    deriving (Show, Eq)

outTokens :: OutputMatch -> [T.Text]
outTokens (OutputMatch fp ln l cs) = cToken <$> cs
{-# INLINE outTokens #-}

insertIndex :: UV.Vector Int -> Int -> Int
insertIndex vs x = search vs 0 (UV.length vs)
  where
    search xs !lo !hi
        | lo == hi = lo
        | otherwise =
            let !mid = (lo + hi) `quot` 2
             in if x < VU.unBox (xs `GV.basicUnsafeIndexM` mid)
                    then search xs lo mid
                    else search xs (mid + 1) hi

getLineNumberAndOffset :: UV.Vector Int -> Int -> (# Int, Int #)
getLineNumberAndOffset xs x =
    let idx = insertIndex xs x
     in (# idx, x - xs `UV.unsafeIndex` (idx - 1) #)
{-# INLINE getLineNumberAndOffset #-}

mkOutputMatches :: UV.Vector Int -> OsPath -> T.Text -> T.Text -> [Chunk] -> ReaderIO [OutputMatch]
mkOutputMatches lineOffsets f text multi ts = do
    invert <- invert_match <$> reader opt
    return $
        if invert
            then map (\(MatchLine n xs) -> OutputMatch f n (ls !! fromIntegral (n - 1)) xs) . invertLines (length ls) $ mkMatchLines lineOffsets multi ts
            else map (\(MatchLine n xs) -> OutputMatch f n (ls !! fromIntegral (n - 1)) xs) $ mkMatchLines lineOffsets multi ts
  where
    ls = T.lines text
{-# INLINE mkOutputMatches #-}

mkMatchLines :: UV.Vector Int -> T.Text -> [Chunk] -> [MatchLine]
mkMatchLines lineOffsets _ [] = []
mkMatchLines lineOffsets text ts =
    map mergeGroup $
        groupBy ((==) `on` mlOffset) . sortBy (compare `on` mlOffset) $
            (\chunk -> let (# r, c #) = getLineNumberAndOffset lineOffsets (cOffset chunk) in MatchLine (fromIntegral r) [Chunk (cTyp chunk) (cToken chunk) c]) <$> ts
  where
    mergeGroup :: [MatchLine] -> MatchLine
    mergeGroup [] = error "mergeGroup: empty list"
    mergeGroup ls@(x : _) = MatchLine (mlOffset x) (foldl' (\l m -> l <> mlChunks m) [] ls)

invertLines :: Int -> [MatchLine] -> [MatchLine]
invertLines n xs = filter (\(MatchLine i _) -> i `notElem` idx) $ take n [MatchLine i [] | i <- [1 ..]]
  where
    idx = mlOffset <$> xs
{-# INLINE invertLines #-}

putOutputMatches :: [OutputMatch] -> ReaderIO (Maybe TB.Builder)
putOutputMatches [] = pure Nothing
putOutputMatches out = do
    Env{..} <- ask
    if
        | json opt -> Just <$> jsonOutput out
        | filename_only opt -> Just <$> filenameOutput out
        | otherwise -> Just <$> defaultOutput out

defaultOutput :: [OutputMatch] -> ReaderIO TB.Builder
defaultOutput xs = do
    Env{..} <- ask
    if
        | Options{no_filename = False, no_numbers = False, count = False} <- opt ->
            pure $ mconcat . intersperse (TB.singleton '\n') $ map (\out -> buildFileName conf opt out <> TB.singleton ':' <> buildLineCol opt out <> TB.singleton ':' <> buildTokens opt out <> buildLine conf opt out) xs
        | Options{no_filename = False, no_numbers = True, count = False} <- opt ->
            pure $ mconcat . intersperse (TB.singleton '\n') $ map (\out -> buildFileName conf opt out <> TB.singleton ':' <> buildTokens opt out <> buildLine conf opt out) xs
        | Options{no_filename = True, no_numbers = False, count = False} <- opt ->
            pure $ mconcat . intersperse (TB.singleton '\n') $ map (\out -> buildTokens opt out <> buildLine conf opt out) xs
        | Options{no_filename = True, no_numbers = True, count = False} <- opt ->
            pure $ mconcat . intersperse (TB.singleton '\n') $ map (\out -> buildTokens opt out <> buildLine conf opt out) xs
        | Options{no_filename = False, count = True} <- opt ->
            do
                let gs = groupBy (\(OutputMatch f1 _ _ _) (OutputMatch f2 _ _ _) -> f1 == f2) xs
                pure $ mconcat . intersperse (TB.singleton '\n') $ (\ys@(y : _) -> buildFileName conf opt y <> TB.singleton ':' <> TB.decimal (length ys)) <$> gs
        | Options{count = True} <- opt ->
            do
                let gs = groupBy (\(OutputMatch f1 _ _ _) (OutputMatch f2 _ _ _) -> f1 == f2) xs
                pure $ mconcat . intersperse (TB.singleton '\n') $ (\ys@(y : _) -> TB.decimal (length ys)) <$> gs

jsonOutput :: [OutputMatch] -> ReaderIO TB.Builder
jsonOutput [] = pure mempty
jsonOutput outs@(OutputMatch fname _ _ _ : _) = do
    strname <- liftIO $ decodeUtf fname
    pure $
        mconcat . intersperse (TB.singleton '\n') $
            [TB.fromString "{ \"file\":\"" <> TB.fromString strname <> TB.fromString "\", \"matches\":["]
                <> [mconcat $ intersperse (TB.singleton ',') (foldl mkMatch [] outs)]
                <> [TB.fromString "]}"]
  where
    mkJToken chunk = TB.fromString "{ \"col\":" <> TB.decimal (cOffset chunk) <> TB.fromString ", \"token\":\"" <> TB.fromText (cToken chunk) <> TB.fromString "\" }"
    mkMatch xs (OutputMatch _ n _ ts) =
        xs
            <> [ TB.fromString "{ \"row\": "
                    <> TB.decimal n
                    <> TB.fromString ", \"tokens\":["
                    <> mconcat (intersperse (TB.fromString ",") (map mkJToken ts))
                    <> TB.fromString "] }"
               ]

filenameOutput :: [OutputMatch] -> ReaderIO TB.Builder
filenameOutput outs = return $ mconcat . intersperse (TB.singleton '\n') $ TB.fromText <$> nub ((\(OutputMatch fname _ _ _) -> (OS.toText fname)) <$> outs)
{-# INLINE filenameOutput #-}

bold, reset :: T.Text
bold = T.pack $ setSGRCode [SetConsoleIntensity BoldIntensity]
reset = T.pack $ setSGRCode []
{-# NOINLINE bold #-}
{-# NOINLINE reset #-}

boldBuilder, resetBuilder :: TB.Builder
boldBuilder = TB.fromText bold
resetBuilder = TB.fromText reset
{-# NOINLINE boldBuilder #-}
{-# NOINLINE resetBuilder #-}

type ColorCode = T.Text

buildFileName :: Config -> Options -> OutputMatch -> TB.Builder
buildFileName conf opt out =
    let str = OS.toText (outFilePath out)
     in buildFileName' conf opt $ str
  where
    buildFileName' :: Config -> Options -> T.Text -> TB.Builder
    buildFileName' conf opt = buildColoredAs opt $ T.pack (setSGRCode (configColorFile conf))
{-# INLINE buildFileName #-}


buildColoredAs :: Options -> ColorCode -> T.Text -> TB.Builder
buildColoredAs Options{color = c, no_color = c'} colorCode txt
    | c && not c' = TB.fromText colorCode <> TB.fromText txt <> resetBuilder
    | otherwise = TB.fromText txt
{-# INLINE buildColoredAs #-}


buildLineCol :: Options -> OutputMatch -> TB.Builder
buildLineCol Options{no_numbers = True} _ = mempty
buildLineCol Options{no_numbers = False, no_column = True} (OutputMatch _ n _ _) = TB.decimal n
buildLineCol Options{no_numbers = False, no_column = False} (OutputMatch _ n _ []) = TB.decimal n
buildLineCol Options{no_numbers = False, no_column = False} (OutputMatch _ n _ (t : _)) = TB.decimal n <> TB.singleton ':' <> TB.decimal ((+ 1) . cOffset $ t)
{-# INLINE buildLineCol #-}

buildTokens :: Options -> OutputMatch -> TB.Builder
buildTokens Options{show_match = st} out
    | st = boldBuilder <> mconcat (TB.fromText <$> outTokens out) <> resetBuilder <> TB.singleton ':'
    | otherwise = mempty

buildLine :: Config -> Options -> OutputMatch -> TB.Builder
buildLine conf Options{color = c, no_color = c'} out
    | c && not c' = highlightLine conf (sortBy (flip compare `on` (T.length . cToken)) (outChunks out)) (outLine out)
    | otherwise = TB.fromText $ outLine out
{-# INLINE buildLine #-}

showFileName :: Config -> Options -> OsPath -> T.Text
showFileName conf opt path = showColoredAs opt (T.pack (setSGRCode (configColorFile conf))) (OS.toText path)
{-# INLINE showFileName #-}

showBold :: Options -> T.Text -> T.Text
showBold opt = showColoredAs opt bold
{-# INLINE showBold #-}

showColoredAs :: Options -> T.Text -> T.Text -> T.Text
showColoredAs Options{color = c, no_color = c'} colorCode str
    | c && not c' = colorCode <> str <> reset
    | otherwise = str
{-# INLINE showColoredAs #-}

highlightLine :: Config -> [Chunk] -> T.Text -> TB.Builder
highlightLine conf ts = highlightLine' (highlightIndexes ts, 0, 0)
  where
    highlightLine' :: ([(Int64, Int64)], Int64, Int) -> T.Text -> TB.Builder
    highlightLine' _ (T.uncons -> Nothing) = mempty
    highlightLine' (ns, !n, !bs) s@(T.uncons -> Just (x, _)) =
        ( if
            | check && bs' == 0 -> if fst stack > 0 then TB.fromString colorMatch <> TB.singleton x <> resetBuilder else TB.singleton x <> resetBuilder
            | check && bs' > 0 -> TB.fromString colorMatch <> TB.singleton x
            | otherwise -> TB.fromText next
        )
            <> highlightLine' (ns, n + nn, bs') rest
      where
        stack = foldr (\(a, b) (c, d) -> (c + fromEnum (a == n), d + fromEnum (b == n))) (0, 0) ns
        check = fst stack > 0 || snd stack > 0
        colorMatch = setSGRCode (configColorMatch conf)
        bs' = bs + fst stack - snd stack
        plain = nub . sort $ foldr (\(a, b) acc -> a : b : acc) [] ns
        nn
            | check = 1
            | [] <- plain' = fromIntegral (T.length s)
            | (p : _) <- plain' = p - n
          where
            plain' = dropWhile (<= n) plain
        (next, rest) = T.splitAt (fromIntegral nn) s
    highlightLine' _ _ = undefined

highlightIndexes :: [Chunk] -> [(Int64, Int64)]
highlightIndexes = foldr (\chunk a -> let b = cOffset chunk in (fromIntegral b, fromIntegral $ b + (T.length (cToken chunk)) - 1) : a) [] . filter (not . T.null . cToken)
{-# INLINE highlightIndexes #-}

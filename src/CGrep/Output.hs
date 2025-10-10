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
{-# LANGUAGE ExistentialQuantification #-}

module CGrep.Output (
    Output (..),
    mkOutputElements,
    putOutputElements,
    runSearch,
    showFileName,
    showBold,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Unsafe as BU

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
import CGrep.Types (Offset, Text8)

import Config (Config (configColorFile, configColorMatch))
import Data.ByteString.Internal (c2w)
import Data.Int (Int64)
import qualified Data.Vector.Fusion.Util as VU (Box (..))
import Data.Word (Word8)
import Reader (Env (..), ReaderIO)

import System.OsPath (OsPath, decodeUtf)
import System.OsString.Internal.Types

import CGrep.Parser.Line (getLineOffsets)
import qualified Data.Vector.Generic as GV
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
import Data.ByteString.Short (ShortByteString)
import System.OsString.Data.ByteString.Short (fromShort)

data Output = Output
    { outFilePath :: OsPath
    , outLineNumb :: {-# UNPACK #-} !Int64
    , outLine :: {-# UNPACK #-} !Text8
    , outChunks :: ![Chunk]
    }

outTokens :: Output -> [Text8]
outTokens (Output fp ln l cs) = cToken <$> cs
{-# INLINE outTokens #-}

insertIndex :: UV.Vector Offset -> Offset -> Int
insertIndex vs x = search vs 0 (UV.length vs)
  where
    search xs !lo !hi
        | lo == hi = lo
        | otherwise =
            let !mid = (lo + hi) `quot` 2
             in if x < VU.unBox (xs `GV.basicUnsafeIndexM` mid)
                    then search xs lo mid
                    else search xs (mid + 1) hi

getLineNumberAndOffset :: UV.Vector Offset -> Offset -> (# Int, Offset #)
getLineNumberAndOffset xs x =
    let idx = insertIndex xs x
     in (# idx, x - xs `UV.unsafeIndex` (idx - 1) #)
{-# INLINE getLineNumberAndOffset #-}

mkOutputElements :: UV.Vector Int64 -> OsPath -> Text8 -> Text8 -> [Chunk] -> ReaderIO [Output]
mkOutputElements lineOffsets f text multi ts = do
    invert <- invert_match <$> reader opt
    return $
        if invert
            then map (\(MatchLine n xs) -> Output f n (ls !! fromIntegral (n - 1)) xs) . invertLines (length ls) $ mkMatchLines lineOffsets multi ts
            else map (\(MatchLine n xs) -> Output f n (ls !! fromIntegral (n - 1)) xs) $ mkMatchLines lineOffsets multi ts
  where
    ls = C.lines text
{-# INLINE mkOutputElements #-}

mkMatchLines :: UV.Vector Int64 -> Text8 -> [Chunk] -> [MatchLine]
mkMatchLines lineOffsets _ [] = []
mkMatchLines lineOffsets text ts =
    map mergeGroup $
        groupBy ((==) `on` lOffset) . sortBy (compare `on` lOffset) $
            (\chunk -> let (# r, c #) = getLineNumberAndOffset lineOffsets (cOffset chunk) in MatchLine (fromIntegral r) [Chunk (cTyp chunk) (cToken chunk) c]) <$> ts
  where
    mergeGroup :: [MatchLine] -> MatchLine
    mergeGroup ls = MatchLine ((lOffset . head) ls) (foldl' (\l m -> l <> lChunks m) [] ls)

invertLines :: Int -> [MatchLine] -> [MatchLine]
invertLines n xs = filter (\(MatchLine i _) -> i `notElem` idx) $ take n [MatchLine i [] | i <- [1 ..]]
  where
    idx = lOffset <$> xs
{-# INLINE invertLines #-}

putOutputElements :: [Output] -> ReaderIO (Maybe B.Builder)
putOutputElements [] = pure Nothing
putOutputElements out = do
    Env{..} <- ask
    if
        | json opt -> Just <$> jsonOutput out
        | filename_only opt -> Just <$> filenameOutput out
        | otherwise -> Just <$> defaultOutput out

runSearch ::
    Options ->
    OsPath ->
    Bool ->
    ReaderIO [Output] ->
    ReaderIO [Output]
runSearch opt filename eligible doSearch =
    if eligible || no_shallow opt
        then doSearch
        else mkOutputElements UV.empty filename C.empty C.empty ([] :: [Chunk])

defaultOutput :: [Output] -> ReaderIO B.Builder
defaultOutput xs = do
    Env{..} <- ask
    if
        | Options{no_filename = False, no_numbers = False, count = False} <- opt ->
            pure $ mconcat . intersperse (B.char8 '\n') $ map (\out -> buildFileName conf opt out <> B.char8 ':' <> buildLineCol opt out <> B.char8 ':' <> buildTokens opt out <> buildLine conf opt out) xs
        | Options{no_filename = False, no_numbers = True, count = False} <- opt ->
            pure $ mconcat . intersperse (B.char8 '\n') $ map (\out -> buildFileName conf opt out <> B.char8 ':' <> buildTokens opt out <> buildLine conf opt out) xs
        | Options{no_filename = True, no_numbers = False, count = False} <- opt ->
            pure $ mconcat . intersperse (B.char8 '\n') $ map (\out -> buildTokens opt out <> buildLine conf opt out) xs
        | Options{no_filename = True, no_numbers = True, count = False} <- opt ->
            pure $ mconcat . intersperse (B.char8 '\n') $ map (\out -> buildTokens opt out <> buildLine conf opt out) xs
        | Options{no_filename = False, count = True} <- opt ->
            do
                let gs = groupBy (\(Output f1 _ _ _) (Output f2 _ _ _) -> f1 == f2) xs
                pure $ mconcat . intersperse (B.char8 '\n') $ (\ys@(y : _) -> buildFileName conf opt y <> B.char8 ':' <> B.intDec (length ys)) <$> gs
        | Options{count = True} <- opt ->
            do
                let gs = groupBy (\(Output f1 _ _ _) (Output f2 _ _ _) -> f1 == f2) xs
                pure $ mconcat . intersperse (B.char8 '\n') $ (\ys@(y : _) -> B.intDec (length ys)) <$> gs

jsonOutput :: [Output] -> ReaderIO B.Builder
jsonOutput [] = pure mempty
jsonOutput outs = do
    strname <- liftIO $ decodeUtf fname
    pure $
        mconcat . intersperse (B.char8 '\n') $
            [B.byteString "{ \"file\":\"" <> B.string8 strname <> B.byteString "\", \"matches\":["]
                <> [mconcat $ intersperse (B.char8 ',') (foldl mkMatch [] outs)]
                <> [B.byteString "]}"]
  where
    fname | (Output f _ _ _) <- head outs = f
    mkJToken chunk = B.byteString "{ \"col\":" <> B.int64Dec (cOffset chunk) <> B.byteString ", \"token\":\"" <> B.byteString (cToken chunk) <> B.byteString "\" }"
    mkMatch xs (Output _ n _ ts) =
        xs
            <> [ B.byteString "{ \"row\": "
                    <> B.int64Dec n
                    <> B.byteString ", \"tokens\":["
                    <> mconcat (intersperse (B.byteString ",") (map mkJToken ts))
                    <> B.byteString "] }"
               ]

filenameOutput :: [Output] -> ReaderIO B.Builder
filenameOutput outs = return $ mconcat . intersperse (B.char8 '\n') $ B.shortByteString <$> nub ((\(Output fname _ _ _) -> (toShortByteString fname)) <$> outs)
{-# INLINE filenameOutput #-}

bold, reset :: C.ByteString
bold = C.pack $ setSGRCode [SetConsoleIntensity BoldIntensity]
reset = C.pack $ setSGRCode []
{-# NOINLINE bold #-}
{-# NOINLINE reset #-}

boldBuilder, resetBuilder :: B.Builder
boldBuilder = B.byteString bold
resetBuilder = B.byteString reset
{-# NOINLINE boldBuilder #-}
{-# NOINLINE resetBuilder #-}

type ColorString = C.ByteString

buildFileName :: Config -> Options -> Output -> B.Builder
buildFileName conf opt out =
    let str = toShortByteString (outFilePath out)
    in buildFileName' conf opt $ str
  where
    buildFileName' :: Config -> Options -> ShortByteString -> B.Builder
    buildFileName' conf opt = buildColoredAs opt $ C.pack (setSGRCode (configColorFile conf))
{-# INLINE buildFileName #-}

buildColoredAs :: Options -> ColorString -> ShortByteString -> B.Builder
buildColoredAs Options{color = c, no_color = c'} colorCode str
    | c && not c' = B.byteString colorCode <> B.shortByteString str <> resetBuilder
    | otherwise = B.shortByteString str
{-# INLINE buildColoredAs #-}

buildLineCol :: Options -> Output -> B.Builder
buildLineCol Options{no_numbers = True} _ = mempty
buildLineCol Options{no_numbers = False, no_column = True} (Output _ n _ _) = B.int64Dec n
buildLineCol Options{no_numbers = False, no_column = False} (Output _ n _ []) = B.int64Dec n
buildLineCol Options{no_numbers = False, no_column = False} (Output _ n _ ts) = B.int64Dec n <> B.char8 ':' <> B.int64Dec ((+ 1) . cOffset . head $ ts)
{-# INLINE buildLineCol #-}

buildTokens :: Options -> Output -> B.Builder
buildTokens Options{show_match = st} out
    | st = boldBuilder <> mconcat (B.byteString <$> outTokens out) <> resetBuilder <> B.char8 ':'
    | otherwise = mempty

buildLine :: Config -> Options -> Output -> B.Builder
buildLine conf Options{color = c, no_color = c'} out
    | c && not c' = highlightLine conf (sortBy (flip compare `on` (C.length . cToken)) (outChunks out)) (outLine out)
    | otherwise = B.byteString $ outLine out
{-# INLINE buildLine #-}

showFileName :: Config -> Options -> OsPath -> C.ByteString
showFileName conf opt path = showColoredAs opt (C.pack (setSGRCode (configColorFile conf))) ((fromShort . toShortByteString) path)
{-# INLINE showFileName #-}

showBold :: Options -> C.ByteString -> C.ByteString
showBold opt = showColoredAs opt bold
{-# INLINE showBold #-}

showColoredAs :: Options -> C.ByteString -> C.ByteString -> C.ByteString
showColoredAs Options{color = c, no_color = c'} colorCode str
    | c && not c' = colorCode <> str <> reset
    | otherwise = str
{-# INLINE showColoredAs #-}

highlightLine :: Config -> [Chunk] -> Text8 -> B.Builder
highlightLine conf ts = highlightLine' (highlightIndexes ts, 0, 0)
  where
    highlightLine' :: ([(Int64, Int64)], Int64, Int) -> C.ByteString -> B.Builder
    highlightLine' _ (C.uncons -> Nothing) = mempty
    highlightLine' (ns, !n, !bs) s@(C.uncons -> Just (x, _)) =
        ( if
            | check && bs' == 0 -> if fst stack > 0 then B.string8 colorMatch <> B.char8 x <> resetBuilder else B.char8 x <> resetBuilder
            | check && bs' > 0 -> B.string8 colorMatch <> B.char8 x
            | otherwise -> B.byteString next
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
            | null plain' = fromIntegral (C.length s)
            | otherwise = head plain' - n
          where
            plain' = dropWhile (<= n) plain
        (next, rest) = C.splitAt (fromIntegral nn) s
    highlightLine' _ _ = undefined

highlightIndexes :: [Chunk] -> [(Int64, Int64)]
highlightIndexes = foldr (\chunk a -> let b = cOffset chunk in (fromIntegral b, b + fromIntegral (C.length (cToken chunk)) - 1) : a) [] . filter (not . B.null . cToken)
{-# INLINE highlightIndexes #-}

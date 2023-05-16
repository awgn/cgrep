-- Copyright (c) 2013-2022 Nicola Bonelli <nicola@pfq.io>
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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module CGrep.Output ( Output(..)
                    , mkOutputElements
                    , putOutputElements
                    , showFileName
                    , showBold) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import qualified Data.Vector.Unboxed as UV
import Data.Vector.Unboxed ( (!) )

import System.Console.ANSI
    ( setSGRCode,
      ConsoleIntensity(BoldIntensity),
      SGR(SetConsoleIntensity) )

import Control.Monad.Trans.Reader ( ask, reader )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Data.List
    ( foldl', sortBy, groupBy, isPrefixOf, nub, sort, genericLength, intersperse )
import Data.Function ( on )

import CGrep.Types ( Text8, Offset )
import CGrep.Chunk ( MatchingLine(..), Chunk(..) )

import Options
    ( Options(Options, invert_match, filename_only, json,
              no_filename, count, no_numbers, no_column, show_match,
              color, no_color) )

import Config ( Config(configColorFile, configColorMatch) )
import Reader ( OptionIO, Env(..) )
import Data.Int ( Int64 )
import Data.Word ( Word8 )
import Data.ByteString.Internal (c2w)
import Debug.Trace
import qualified Data.Vector.Unboxed as VU

data Output = Output
    { outFilePath :: B.ByteString
    , outLineNumb :: {-# UNPACK #-} !Int64
    , outLine     :: {-# UNPACK #-} !Text8
    , outChunks   :: ![Chunk]
    }
    deriving (Show)


-- Returns a vector of offsets for a given character in a ByteString, up to the given maximum offset.
charOffsets :: Char -> Int64 -> B.ByteString -> UV.Vector Int64
charOffsets c maxOff bs = UV.unfoldrN (fromIntegral maxOff) findOffsets 0
  where
    target = c2w c
    findOffsets :: Int64 -> Maybe (Int64, Int64)
    findOffsets i
      | i >= maxOff = Nothing
      | B.index bs (fromIntegral i) == target = Just (fromIntegral i, i + 1)
      | otherwise = findOffsets (i + 1)


getLineOffsets :: Text8 -> Int64 -> UV.Vector Offset
getLineOffsets text maxOff =
    let l = C.length text
        idx = charOffsets '\n' maxOff text
    in if VU.null idx
        then idx
        else if UV.last idx == fromIntegral (l-1)
            then UV.init idx
            else idx


insertIndex :: UV.Vector Offset -> Offset -> Int
insertIndex xs x = search xs 0 (UV.length xs)
    where
        search xs lo hi
            | lo == hi = lo
            | otherwise = let mid = (lo + hi) `div` 2
                        in if x < xs ! mid
                                then search xs lo mid
                                else search xs (mid+1) hi


getLineNumberAndOffset :: UV.Vector Offset -> Offset -> (# Int, Offset #)
getLineNumberAndOffset xs x =
    let idx = insertIndex xs x
        shift = if idx == 0 then 0 else 1
    in (# idx, x - shift - (if idx > 0 then xs ! (idx-1) else 0) #)
{-# INLINE getLineNumberAndOffset #-}


mkOutputElements :: FilePath -> Text8 -> Text8 -> [Chunk] -> OptionIO [Output]
mkOutputElements f text multi ts = do
    invert <- invert_match <$> reader opt
    return $ if invert then map (\(MatchingLine n xs) -> Output fp n (ls !! fromIntegral (n-1)) xs) . invertLines (length ls) $ mkMatchingLines multi ts
                       else map (\(MatchingLine n xs) -> Output fp n (ls !! fromIntegral (n-1)) xs) $ mkMatchingLines multi ts
    where ls = C.lines text
          fp = C.pack f
{-# INLINE mkOutputElements #-}


mkMatchingLines :: Text8 -> [Chunk] -> [MatchingLine]
mkMatchingLines _ [] = []
mkMatchingLines text ts = map mergeGroup $ groupBy ((==) `on` lOffset) . sortBy (compare `on` lOffset) $
    (\Chunk{..} -> let (# r, c #) = getLineNumberAndOffset lineOffsets tOffset in MatchingLine (fromIntegral r + 1) [Chunk c tStr]) <$> ts
        where mergeGroup :: [MatchingLine] -> MatchingLine
              mergeGroup ls = MatchingLine ((lOffset . head) ls) (foldl' (\l m -> l <> lChunks m) [] ls)
              maxOff = maximum $ tOffset <$> ts
              lineOffsets = getLineOffsets text maxOff


invertLines :: Int -> [MatchingLine] -> [MatchingLine]
invertLines n xs =  filter (\(MatchingLine i _) ->  i `notElem` idx ) $ take n [ MatchingLine i [] | i <- [1..]]
    where idx = lOffset <$> xs
{-# INLINE invertLines #-}


putOutputElements :: [Output] -> OptionIO [B.Builder]
putOutputElements out = do
    Env{..} <- ask
    if  | json opt          -> jsonOutput out
        | filename_only opt -> filenameOutput out
        | otherwise         -> defaultOutput out


defaultOutput :: [Output] -> OptionIO [B.Builder]
defaultOutput xs = do
    Env{..} <- ask
    if |  Options{ no_filename = False, no_numbers = False , count = False } <- opt
                -> return $ map (\out -> buildFileName conf opt out <> B.char8 ':' <> buildLineCol opt out <> B.char8 ':' <> buildTokens opt out <> buildLine conf opt out) xs

        |  Options{ no_filename = False, no_numbers = True  , count = False } <- opt
                -> return $ map (\out -> buildFileName conf opt out <> B.char8 ':' <> buildTokens opt out <> buildLine conf opt out) xs

        |  Options{ no_filename = True , no_numbers = False , count = False } <- opt
              -> return $ map (\out -> buildTokens opt out <> buildLine conf opt out) xs

        |  Options{ no_filename = True , no_numbers = True  , count = False } <- opt
              -> return $ map (\out -> buildTokens opt out <> buildLine conf opt out) xs

        |  Options{ no_filename = False, count = True } <- opt
            -> do
                let gs = groupBy (\(Output f1 _ _ _) (Output f2 _ _ _) -> f1 == f2) xs
                return $ (\ys@(y:_) -> buildFileName conf opt y <> B.char8 ':' <> B.intDec (length ys)) <$> gs

        |  Options{ count = True } <- opt
            -> do
                let gs = groupBy (\(Output f1 _ _ _) (Output f2 _ _ _) -> f1 == f2) xs
                return $ (\ys@(y:_) ->  B.intDec (length ys)) <$> gs


jsonOutput :: [Output] -> OptionIO [B.Builder]
jsonOutput [] = return []
jsonOutput outs = return $
    [B.byteString "{ \"file\":\"" <> B.byteString fname <> B.byteString "\", \"matches\":["] <>
    [ mconcat $ intersperse (B.char8 ',') (foldl mkMatch [] outs) ] <> [B.byteString "]}"]
     where fname | (Output f _ _ _) <- head outs = f
           mkJToken (Chunk n xs) = B.byteString "{ \"col\":" <> B.int64Dec n <> B.byteString ", \"token\":\"" <> B.byteString xs <> B.byteString "\" }"
           mkMatch xs (Output _ n _ ts) =
               xs <> [B.byteString "{ \"row\": " <> B.int64Dec n <> B.byteString ", \"tokens\":[" <>
                        mconcat (intersperse (B.byteString ",") (map mkJToken ts)) <> B.byteString "] }" ]


filenameOutput :: [Output] -> OptionIO [B.Builder]
filenameOutput outs = return $ B.byteString <$> nub ((\(Output fname _ _ _) -> fname) <$> outs)
{-# INLINE filenameOutput #-}


bold, reset :: String
bold  = setSGRCode [SetConsoleIntensity BoldIntensity]
reset = setSGRCode []
{-# INLINE bold #-}
{-# INLINE reset #-}


boldBuilder, resetBuilder :: B.Builder
boldBuilder = B.string8 bold
resetBuilder = B.string8 reset
{-# INLINE boldBuilder #-}
{-# INLINE resetBuilder #-}


type ColorString = String


buildFileName :: Config -> Options -> Output -> B.Builder
buildFileName conf opt = buildFileName' conf opt . outFilePath
    where buildFileName' :: Config -> Options -> B.ByteString -> B.Builder
          buildFileName' conf opt = buildColoredAs opt $ setSGRCode (configColorFile conf)
{-# INLINE buildFileName #-}


buildColoredAs :: Options -> ColorString -> B.ByteString -> B.Builder
buildColoredAs Options { color = c, no_color = c'} colorCode str
    | c && not c'= B.string8 colorCode <> B.byteString str <> resetBuilder
    | otherwise  = B.byteString str
{-# INLINE buildColoredAs #-}


buildLineCol :: Options -> Output -> B.Builder
buildLineCol Options{no_numbers = True } _ = mempty
buildLineCol Options{no_numbers = False, no_column = True  } (Output _ n _ _)  = B.int64Dec n
buildLineCol Options{no_numbers = False, no_column = False } (Output _ n _ []) = B.int64Dec n
buildLineCol Options{no_numbers = False, no_column = False } (Output _ n _ ts) = B.int64Dec n <> B.char8 ':' <> B.int64Dec ((+1) . tOffset . head $ ts)
{-# INLINE buildLineCol #-}


buildTokens :: Options -> Output -> B.Builder
buildTokens Options { show_match = st } out
    | st        = boldBuilder <> mconcat (B.byteString . tStr <$> outChunks out) <> resetBuilder <> B.char8 ':'
    | otherwise = mempty


buildLine :: Config -> Options -> Output -> B.Builder
buildLine conf Options { color = c, no_color = c' } out
    | c && not c'= highlightLine conf (sortBy (flip compare `on` (C.length . tStr)) (outChunks out)) (outLine out)
    | otherwise  = B.byteString $ outLine out
{-# INLINE buildLine #-}


showFileName :: Config -> Options -> String -> String
showFileName conf opt = showColoredAs opt $ setSGRCode (configColorFile conf)
{-# INLINE showFileName #-}


showBold :: Options -> String -> String
showBold opt = showColoredAs opt bold
{-# INLINE showBold #-}


showColoredAs :: Options -> String -> String -> String
showColoredAs Options { color = c, no_color = c'} colorCode str
    | c && not c'= colorCode <> str <> reset
    | otherwise  = str
{-# INLINE showColoredAs #-}


highlightLine :: Config -> [Chunk] -> Text8 -> B.Builder
highlightLine conf ts =  highlightLine' (highlightIndexes ts, 0, 0)
    where highlightLine' :: ([(Int64, Int64)], Int64, Int) -> C.ByteString -> B.Builder
          highlightLine'  _ (C.uncons -> Nothing) = mempty
          highlightLine' (ns, !n, !bs) s@(C.uncons -> Just (x,_)) =
                (if | check && bs' == 0 -> if fst stack > 0 then B.string8 colorMatch <> B.char8 x <> resetBuilder else B.char8 x <> resetBuilder
                    | check && bs' > 0 -> B.string8 colorMatch <> B.char8 x
                    | otherwise -> B.byteString next) <> highlightLine' (ns, n + nn, bs') rest
            where stack = foldr (\(a, b) (c, d) -> (c + fromEnum (a == n), d + fromEnum (b == n))) (0, 0) ns
                  check = fst stack > 0 || snd stack > 0
                  colorMatch = setSGRCode (configColorMatch conf)
                  bs' = bs + fst stack - snd stack
                  plain = nub . sort $ foldr (\(a, b) acc -> a : b : acc) [] ns
                  nn | check = 1
                     | null plain' = fromIntegral (C.length s)
                     | otherwise = head plain' - n
                         where plain' = dropWhile (<=n) plain
                  (next, rest) = C.splitAt (fromIntegral nn) s
          highlightLine'  _ _ = undefined


highlightIndexes :: [Chunk] -> [(Int64, Int64)]
highlightIndexes = foldr (\Chunk{..} a -> let b = tOffset in (fromIntegral b, b + fromIntegral (C.length tStr) - 1) : a) [] . filter (not. B.null.tStr)
{-# INLINE highlightIndexes #-}

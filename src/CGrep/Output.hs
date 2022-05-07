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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module CGrep.Output ( Output(..)
                    , mkOutput
                    , putOutputHeader
                    , putOutputFooter
                    , putOutput
                    , showFileName
                    , showFile
                    , showBold) where

import qualified Data.ByteString as B

import qualified Data.ByteString.Builder as B

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import qualified Codec.Binary.UTF8.String as UC

import System.Console.ANSI
    ( setSGRCode,
      ConsoleIntensity(BoldIntensity),
      SGR(SetConsoleIntensity) )

import Control.Monad.Trans.Reader ( ask, reader )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Control.Applicative

import Data.List
    ( foldl', sortBy, groupBy, isPrefixOf, nub, sort, genericLength, intersperse )
import Data.Function ( on )

import CGrep.Types ( Text8, LineOffset, Offset2d, Offset )
import CGrep.Token ( Line(..), Token(..) )

import Options
    ( Options(Options, invert_match, filename_only, json, xml,
              no_filename, count, no_numbers, no_column, show_match,
              color, no_color) )

import Config ( Config(configColorFile, configColorMatch) )
import Reader ( OptionIO )
import Data.Int ( Int64 )
import Data.Containers.ListUtils


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
    return $ if invert then map (\(Line n xs) -> Output f n (ls !! fromIntegral (n-1)) xs) . invertLines (length ls) $ mkLines multi ts
                       else map (\(Line n xs) -> Output f n (ls !! fromIntegral (n-1)) xs) $ mkLines multi ts
    where ls = C.lines text
{-# INLINE mkOutput #-}


mkLines :: Text8 -> [Token] -> [Line]
mkLines _ [] = []
mkLines text ts = map mergeGroup $ groupBy ((==) `on` lOffset) $
    sortBy (compare `on` lOffset) $ (\Token{..} -> let (# r, c #) = getOffset2d ols tOffset in Line (1 + r) [Token c tStr]) <$> ts
    where mergeGroup :: [Line] -> Line
          mergeGroup ls = Line ((lOffset . head) ls) (foldl (\l m -> l ++ lTokens m) [] ls)
          ols = getOffsetsLines text


invertLines :: Int -> [Line] -> [Line]
invertLines n xs =  filter (\(Line i _) ->  i `notElem` idx ) $ take n [ Line i [] | i <- [1..]]
    where idx = lOffset <$> xs
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


putOutput :: [Output] -> OptionIO [B.Builder]
putOutput out = do
    (_,opt) <- ask
    if  | xml opt           -> xmlOutput  out
        | json opt          -> jsonOutput out
        | filename_only opt -> filenameOutput out
        | otherwise         -> defaultOutput out

space :: B.Builder
space = B.char8 ' '

defaultOutput :: [Output] -> OptionIO [B.Builder]
defaultOutput xs = do
    (conf,opt) <- ask
    if |  Options{ no_filename = False, no_numbers = False , count = False } <- opt
                -> return $ map (\out -> buildFile conf opt out <> B.char8 ':' <> buildLineCol opt out <> space <> buildTokens opt out <> space <> buildLine conf opt out) xs

        |  Options{ no_filename = False, no_numbers = True  , count = False } <- opt
                -> return $ map (\out -> buildFile conf opt out <> space <> buildTokens opt out <> space <> buildLine conf opt out) xs

        |  Options{ no_filename = True , no_numbers = False , count = False } <- opt
              -> return $ map (\out -> buildTokens opt out <> space <> buildLine conf opt out) xs

        |  Options{ no_filename = True , no_numbers = True  , count = False } <- opt
              -> return $ map (\out -> buildTokens opt out <> space <> buildLine conf opt out) xs

        |  Options{ no_filename = False, count = True } <- opt
            -> do
                let gs = groupBy (\(Output f1 _ _ _) (Output f2 _ _ _) -> f1 == f2) xs
                return $ (\ys@(y:_) -> buildFile conf opt y <> B.char8 ':' <> B.intDec (length ys)) <$> gs

        |  Options{ count = True } <- opt
            -> do
                let gs = groupBy (\(Output f1 _ _ _) (Output f2 _ _ _) -> f1 == f2) xs
                return $ (\ys@(y:_) ->  B.intDec (length ys)) <$> gs


jsonOutput :: [Output] -> OptionIO [B.Builder]
jsonOutput [] = return []
jsonOutput outs = return $
    [B.byteString "{ \"file\":\"" <> B.stringUtf8 fname <> B.byteString "\", \"matches\":["] <>
    [ mconcat $ intersperse (B.char8 ',') (foldl mkMatch [] outs) ] <> [B.byteString "]}"]
     where fname | (Output f _ _ _) <- head outs = f
           mkJToken (Token n xs) = B.byteString "{ \"col\":" <> B.int64Dec n <> B.byteString ", \"token\":\"" <> B.byteString xs <> B.byteString "\" }"
           mkMatch xs (Output _ n _ ts) =
               xs <> [B.byteString "{ \"row\": " <> B.int64Dec n <> B.byteString ", \"tokens\":[" <>
                        mconcat (intersperse (B.byteString ",") (map mkJToken ts)) <> B.byteString "] }" ]


filenameOutput :: [Output] -> OptionIO [B.Builder]
filenameOutput outs = return $ B.stringUtf8 <$> nub ((\(Output fname _ _ _) -> fname) <$> outs)
{-# INLINE filenameOutput #-}


xmlOutput :: [Output] -> OptionIO [B.Builder]
xmlOutput [] = return []
xmlOutput outs = return $
    [B.byteString "<file name='" <> B.stringUtf8  fname <> B.byteString "'>" ] ++
    [B.byteString "<matches>" ] ++
    [foldl mkMatch mempty outs] ++
    [B.byteString "</matches>"] ++
    [B.byteString "</file>"]
        where fname = case outs of
                        [] -> ""
                        (Output f _ _ _) : _ -> f
              mkToken (Token n xs) = B.byteString "<token col='" <> B.int64Dec n <> B.byteString "'>" <> B.byteString xs <> B.byteString "</token>"
              mkMatch xs (Output _ n _ ts) =
                  xs <> B.byteString "<match line='" <> B.int64Dec n <> B.byteString "'>" <> mconcat (map mkToken ts) <> B.byteString "</match>"


replace :: String -> [(String, String)] -> String
replace ys@(x:xs) pats =
  let pats' = filter ((`isPrefixOf` ys) . fst) pats  in
  if null pats' then x : replace xs pats
                else let new = head pats' in snd new ++ replace (drop (length(fst new) - 1) xs) pats
replace [] _ = []


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


buildFile :: Config -> Options -> Output -> B.Builder
buildFile conf opt = buildFileName conf opt . outFilePath
{-# INLINE buildFile #-}


buildFileName :: Config -> Options -> String -> B.Builder
buildFileName conf opt = buildColoredAs opt $ setSGRCode (configColorFile conf)
{-# INLINE buildFileName #-}


buildColoredAs :: Options -> ColorString -> String -> B.Builder
buildColoredAs Options { color = c, no_color = c'} colorCode str
    | c && not c'= B.string8 colorCode <> B.string8 str <> B.string8 resetTerm
    | otherwise  = B.string8 str
{-# INLINE buildColoredAs #-}


showLineCol :: Options -> Output -> String
showLineCol Options{no_numbers = True } _ = ""
showLineCol Options{no_numbers = False, no_column = True  } (Output _ n _ _)  = show n
showLineCol Options{no_numbers = False, no_column = False } (Output _ n _ []) = show n
showLineCol Options{no_numbers = False, no_column = False } (Output _ n _ ts) = show n ++ ":" ++ show ((+1) . tOffset . head $ ts)
{-# INLINE showLineCol #-}


buildLineCol :: Options -> Output -> B.Builder
buildLineCol Options{no_numbers = True } _ = mempty
buildLineCol Options{no_numbers = False, no_column = True  } (Output _ n _ _)  = B.int64Dec n
buildLineCol Options{no_numbers = False, no_column = False } (Output _ n _ []) = B.int64Dec n
buildLineCol Options{no_numbers = False, no_column = False } (Output _ n _ ts) = B.int64Dec n <> B.char8 ':' <> B.int64Dec ((+1) . tOffset . head $ ts)
{-# INLINE buildLineCol #-}


buildTokens :: Options -> Output -> B.Builder
buildTokens Options { show_match = st } out
    | st        = B.stringUtf8 bold <> mconcat (B.byteString . tStr <$> outTokens out) <> B.stringUtf8 resetTerm
    | otherwise = mempty
{-# INLINE buildTokens #-}


buildLine :: Config -> Options -> Output -> B.Builder
buildLine conf Options { color = c, no_color = c' } out
    | c && not c'= hilightLine conf (sortBy (flip compare `on` (C.length . tStr)) (outTokens out)) (outLine out)
    | otherwise  = B.byteString $ outLine out
{-# INLINE buildLine #-}


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


hilightLine :: Config -> [Token] -> Text8 -> B.Builder
hilightLine conf ts =  hilightLine' (hilightIndicies ts, 0, 0)
    where hilightLine' :: ([(Int64, Int64)], Int64, Int) -> C.ByteString -> B.Builder
          hilightLine'  _ (C.uncons -> Nothing) = mempty
          hilightLine' (ns, !n, !bs) s@(C.uncons -> Just (x,_)) =
                (if | check && bs' == 0 -> if fst stack > 0 then B.string8 colorMatch <> B.char8 x <> B.string8 resetTerm else B.char8 x <> B.string8 resetTerm
                    | check && bs' > 0 -> B.string8 colorMatch <> B.char8 x
                    | otherwise -> B.byteString next) <> hilightLine' (ns, n + nn, bs') rest
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
          hilightLine'  _ _ = undefined


hilightIndicies :: [Token] -> [(Int64, Int64)]
hilightIndicies = foldr (\Token{..} a -> let b = tOffset in (fromIntegral b, b + fromIntegral (C.length tStr) - 1) : a) [] . filter (not. B.null.tStr)
{-# INLINE hilightIndicies #-}

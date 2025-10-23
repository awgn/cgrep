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

module CGrep.Match (
    Match (..),
    mkMatches,
    putMatches,
    prettyFileName,
    prettyBold,
) where

import CGrep.Line (LineIndex, getLineByOffset', getLineOffsets, lookupLineAndPosition, totalLines)
import System.OsPath
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Unsafe as TU
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLB

import CGrep.Parser.Chunk (Chunk (..), MatchLine (..), cOffset)
import Reader (ReaderIO, Env (..))
import Config
import Options (Options(..))
import qualified OsPath as OS
import Data.Function (on)
import Data.List (sortBy, groupBy)
import Data.List.Extra (intersperse)
import Control.Monad.Reader
import System.Console.ANSI (setSGRCode)
import System.Console.ANSI.Codes (ConsoleIntensity(..))
import System.Console.ANSI (SGR(..))
import Data.List (sort, nub)
import CGrep.Text (textOffsetWord8)
import Data.List (sortOn)

data Match = Match
    { mFilePath :: OsPath
    , mLineNumb :: {-# UNPACK #-} !Int
    , mLine :: {-# UNPACK #-} !T.Text
    , mChunks :: ![Chunk]
    }
    deriving stock (Show, Eq)

mTokens :: Match -> [T.Text]
mTokens (Match fp ln l cs) = cToken <$> cs
{-# INLINE mTokens #-}


mkMatches :: LineIndex -> OsPath -> T.Text -> [Chunk] -> ReaderIO [Match]
mkMatches lindex f txt chunks = do
    invert <- invert_match <$> reader opt
    return $
        if invert
            then
                map
                    ( \(MatchLine n xs) ->
                        --let line = getLineByOffset' lindex ((cOffset . head) xs)
                         Match f n txt xs
                    )
                    . invertLines (totalLines lindex)
                    $ mkMatchLines lindex chunks
            else
                map
                    ( \(MatchLine n xs) ->
                        let line = getLineByOffset' lindex ((cOffset . head) xs)
                          in Match f n line xs
                    )
                    $ mkMatchLines lindex chunks

mkMatchLines :: LineIndex -> [Chunk] -> [MatchLine]
mkMatchLines _ [] = []
mkMatchLines lindex chunks =
    map mergeGroup $
        groupBy ((==) `on` mlOffset) . sortBy (compare `on` mlOffset) $
            ( \chunk ->
                let (# r, c #) = lookupLineAndPosition lindex (cOffset chunk)
                 in MatchLine r [Chunk (cTyp chunk) (cToken chunk)]
            )
                <$> chunks
  where
    mergeGroup :: [MatchLine] -> MatchLine
    mergeGroup [] = error "mergeGroup: empty list"
    mergeGroup ls@(x : _) = MatchLine (mlOffset x) (foldl' (\l m -> l <> mlChunks m) [] ls)


invertLines :: Int -> [MatchLine] -> [MatchLine]
invertLines n xs = filter (\(MatchLine i _) -> i `notElem` idx) $ take n [MatchLine i [] | i <- [1 ..]]
  where
    idx = mlOffset <$> xs
{-# INLINE invertLines #-}

putMatches :: [Match] -> ReaderIO (Maybe TLB.Builder)
putMatches [] = pure Nothing
putMatches out = do
    Env{..} <- ask
    if
        | null_output opt -> pure Nothing
     -- | filename_only opt -> Just <$> filenameOutput out
        | otherwise -> Just <$> defPutMatches out

--------------------------------------------------------------------

defPutMatches :: [Match] -> ReaderIO TLB.Builder
defPutMatches xs = do
    Env{..} <- ask
    if
        | Options{no_filename = False, no_numbers = False, count = False} <- opt ->
            pure $ mconcat . intersperse (TLB.singleton '\n') $ map (\out -> buildFileName conf opt out <> TLB.singleton ':' <> buildLineCol opt out <> TLB.singleton ':' <> buildTokens opt out <> buildLine conf opt out) xs
        | Options{no_filename = False, no_numbers = True, count = False} <- opt ->
            pure $ mconcat . intersperse (TLB.singleton '\n') $ map (\out -> buildFileName conf opt out <> TLB.singleton ':' <> buildTokens opt out <> buildLine conf opt out) xs
        | Options{no_filename = True, no_numbers = False, count = False} <- opt ->
            pure $ mconcat . intersperse (TLB.singleton '\n') $ map (\out -> buildTokens opt out <> buildLine conf opt out) xs
        | Options{no_filename = True, no_numbers = True, count = False} <- opt ->
            pure $ mconcat . intersperse (TLB.singleton '\n') $ map (\out -> buildTokens opt out <> buildLine conf opt out) xs
        | Options{no_filename = False, count = True} <- opt ->
            do
                let gs = groupBy (\(Match f1 _ _ _) (Match f2 _ _ _) -> f1 == f2) xs
                pure $ mconcat . intersperse (TLB.singleton '\n') $ (\ys@(y : _) -> buildFileName conf opt y <> TLB.singleton ':' <> TLB.decimal (length ys)) <$> gs
        | Options{count = True} <- opt ->
            do
                let gs = groupBy (\(Match f1 _ _ _) (Match f2 _ _ _) -> f1 == f2) xs
                pure $ mconcat . intersperse (TLB.singleton '\n') $ (\ys@(y : _) -> TLB.decimal (length ys)) <$> gs

--------------------------------------------------------------------

buildFileName :: Config -> Options -> Match -> TLB.Builder
buildFileName conf opt out =
    let str = OS.toText (mFilePath out)
     in buildFileName' conf opt $ str
  where
    buildFileName' :: Config -> Options -> T.Text -> TLB.Builder
    buildFileName' conf opt = buildColoredText opt $ T.pack (setSGRCode (configColorFile conf))
{-# INLINE buildFileName #-}

buildColoredText :: Options -> ColorCode -> T.Text -> TLB.Builder
buildColoredText opt colorCode txt
    | color opt && not (no_color opt) = TLB.fromText colorCode <> TLB.fromText txt <> resetBuilder
    | otherwise = TLB.fromText txt
{-# INLINE buildColoredText #-}

buildLineCol :: Options -> Match -> TLB.Builder
buildLineCol Options{no_numbers = True} _ = mempty
buildLineCol Options{no_numbers = False, no_column = True} (Match _ n _ _) = TLB.decimal n
buildLineCol Options{no_numbers = False, no_column = False} (Match _ n _ []) = TLB.decimal n
buildLineCol Options{no_numbers = False, no_column = False} (Match _ n _ (t : _)) = TLB.decimal n <> TLB.singleton ':' <> TLB.decimal ((+ 1) . cOffset $ t)
{-# INLINE buildLineCol #-}

buildTokens :: Options -> Match -> TLB.Builder
buildTokens Options{show_match = st} out
    | st = boldBuilder <> mconcat (TLB.fromText <$> mTokens out) <> resetBuilder <> TLB.singleton ':'
    | otherwise = mempty
{-# INLINE buildTokens #-}

buildLine :: Config -> Options -> Match -> TLB.Builder
buildLine conf Options{color = c, no_color = no_c} out
    | c && not no_c = buildColoredLine conf (sortBy (flip compare `on` (T.length . cToken)) (mChunks out)) (mLine out)
    | otherwise = TLB.fromText $ mLine out
{-# INLINE buildLine #-}

buildColoredLine :: Config -> [Chunk] -> T.Text -> TLB.Builder
buildColoredLine conf chunks line =
  let
    lineOffset = textOffsetWord8 line
    lineByteLen = TU.lengthWord8 line
    lineEndOffset = lineOffset + lineByteLen

    events :: [(Int, Int)]
    events = sortOn fst $ concatMap chunkToEvents chunks

    chunkToEvents :: Chunk -> [(Int, Int)]
    chunkToEvents chunk =
      let
        -- Offset assoluti del chunk
        chunkStartAbs = cOffset chunk
        chunkLen = TU.lengthWord8 (cToken chunk)
        chunkEndAbs = chunkStartAbs + chunkLen

        -- Controlla se il chunk si sovrappone a questa riga
        overlaps = chunkEndAbs > lineOffset && chunkStartAbs < lineEndOffset

      in
        if not overlaps || chunkLen == 0
        then []
        else
          let
            -- Calcola l'inizio relativo, clippato a 0 (inizio riga)
            relStart = max 0 (chunkStartAbs - lineOffset)

            -- Calcola la fine relativa, clippata alla lunghezza della riga
            relEnd = min lineByteLen (chunkEndAbs - lineOffset)

          in
            if relStart < relEnd
            then [(relStart, 1), (relEnd, -1)] -- Evento Inizio, Evento Fine
            else []

    colorMatch = TLB.fromString $ setSGRCode (configColorMatch conf)

    -- 2. Itera sugli eventi (la logica qui non cambia)
    --    Stato: (lastByteIndex, currentHighlightLevel, accumulatedBuilder)
    processEvent :: (Int, Int, TLB.Builder) -> (Int, Int) -> (Int, Int, TLB.Builder)
    processEvent (lastIdx, level, accBuilder) (eventIdx, delta) =
      let
        accBuilder'
          | eventIdx > lastIdx =
              let
                chunkLen = eventIdx - lastIdx
                -- Usa le funzioni efficienti di slicing per byte (sulla riga)
                textChunk = TU.takeWord8 chunkLen (TU.dropWord8 lastIdx line)

                coloredChunk
                  | level > 0 = colorMatch <> TLB.fromText textChunk <> resetBuilder
                  | otherwise = TLB.fromText textChunk
              in
                accBuilder <> coloredChunk
          | otherwise = accBuilder

        newLevel = level + delta
        colorChange
          | newLevel > 0 && level == 0 = colorMatch
          | newLevel == 0 && level > 0 = resetBuilder
          | otherwise                  = mempty

      in
        (eventIdx, newLevel, accBuilder' <> colorChange)

    -- 3. Esegui il fold e gestisci il testo rimanente (la logica qui non cambia)
    (finalIdx, finalLevel, mainBuilder) = foldl' processEvent (0, 0, mempty) events

    remainingText = TU.dropWord8 finalIdx line
    finalBuilder
      | finalLevel > 0 = colorMatch <> TLB.fromText remainingText <> resetBuilder
      | otherwise        = TLB.fromText remainingText

  in
    mainBuilder <> finalBuilder

--------------------------------------------------------------------

prettyFileName :: Config -> Options -> OsPath -> TL.Text
prettyFileName conf opt path = TLB.toLazyText $ buildColoredText opt (T.pack $ setSGRCode (configColorFile conf)) (OS.toText path)
{-# INLINE prettyFileName #-}

prettyBold :: Options -> T.Text -> TL.Text
prettyBold opt txt = TLB.toLazyText $ buildColoredText opt bold txt
{-# INLINE prettyBold #-}

--------------------------------------------------------------------

type ColorCode = T.Text

boldBuilder, resetBuilder :: TLB.Builder
boldBuilder = TLB.fromText bold
resetBuilder = TLB.fromText reset
{-# NOINLINE boldBuilder #-}
{-# NOINLINE resetBuilder #-}


bold, reset :: T.Text
bold = T.pack $ setSGRCode [SetConsoleIntensity BoldIntensity]
reset = T.pack $ setSGRCode []
{-# NOINLINE bold #-}
{-# NOINLINE reset #-}

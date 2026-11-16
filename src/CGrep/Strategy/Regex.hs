--
-- Copyright (c) 2013-2025 Nicola Bonelli <nicola@larthia.com>
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

module CGrep.Strategy.Regex (
    search,
    searchRef,
    mkRegexSearcher,
    extractPosixLinePrefilter,
) where

import Control.Monad.Trans.Reader (ask)

import Text.Regex.Base (
    AllTextMatches (getAllTextMatches),
    MatchText,
    RegexLike (matchAllText),
    RegexMaker (makeRegex),
 )

#ifdef ENABLE_PCRE
import qualified Text.Regex.PCRE ((=~))
#endif
import qualified Text.Regex.TDFA as TDFA

import Data.Array (Array, elems)
import Data.List (delete, maximumBy)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Word (Word8)

import CGrep.Common (
    ignoreCase,
 )
import CGrep.FileType (FileType)
import CGrep.FileTypeMap (FileTypeInfo (..))
import CGrep.FileTypeMapTH (mkContextFilterFn)
import CGrep.Line (buildIndex)
import CGrep.Match (Match, mkMatches, mkMatchesDirect)
import CGrep.Parser.Chunk
import CGrep.Semantic.ContextFilter (mkContextFilter)
import CGrep.Text (firstIndex, indicesFast, textOffsetWord8)

import Options (Options (invert_match))
#ifdef ENABLE_PCRE
import Options (Options (regex_pcre))
#endif

import Control.Concurrent (MVar)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import Data.Text.Internal.ArrayUtils (memchr)
import PutMessage (putMessageLnVerb)
import Reader (Env (..), ReaderIO)
import System.IO (stderr)
import System.OsPath (OsPath)

data CompiledPosixRegex = CompiledPosixRegex
    { cprRegex :: !TDFA.Regex
    , cprPrefilter :: !(Maybe [[T.Text]])
    }

compilePosixPattern :: T.Text -> CompiledPosixRegex
compilePosixPattern pat =
    let !rx = makeRegex pat
        !pf = extractPosixLinePrefilter pat
     in CompiledPosixRegex rx pf

mkRegexSearcher :: Env -> [T.Text] -> (MVar () -> Maybe (FileType, FileTypeInfo) -> OsPath -> T.Text -> Bool -> ReaderIO [Match])
mkRegexSearcher (Env _ opt0) patterns =
#ifdef ENABLE_PCRE
    if regex_pcre opt0
        then \lock info filename text _strict -> search lock info filename text patterns _strict
        else
            let !compiled = map compilePosixPattern patterns
             in \lock info filename text _strict -> searchPosixCompiled compiled lock info filename text
#else
    let !compiled = if null patterns && invert_match opt0 then [] else map compilePosixPattern patterns
     in \lock info filename text _strict -> searchPosixCompiled compiled lock info filename text
#endif

search :: MVar () -> Maybe (FileType, FileTypeInfo) -> OsPath -> T.Text -> [T.Text] -> Bool -> ReaderIO [Match]
search lock info filename text patterns _strict = do
#ifdef ENABLE_PCRE
    Env{..} <- ask
    if regex_pcre opt
        then do
            let !contextFilter = mkContextFilterFn (fst <$> info) (mkContextFilter opt) False
                text' = ignoreCase opt . contextFilter $ text
                tokens =
                    map (\(str, (_, _)) -> Chunk ChunkUnspec str) $
                        concatMap elems $
                            patterns >>= (\p -> elems (getAllTextMatches $ text' Text.Regex.PCRE.=~ p :: Array Int (MatchText T.Text)))
            if not (invert_match opt) && textOffsetWord8 text == 0
                then pure $! mkMatchesDirect filename text tokens
                else mkMatches (buildIndex text) filename text' tokens
        else searchPosixCompiled (map compilePosixPattern patterns) lock info filename text
#else
    searchPosixCompiled (map compilePosixPattern patterns) lock info filename text
#endif

searchPosixCompiled :: [CompiledPosixRegex] -> MVar () -> Maybe (FileType, FileTypeInfo) -> OsPath -> T.Text -> ReaderIO [Match]
searchPosixCompiled compiled lock info filename text = do
    Env{..} <- ask
    let !contextFilter = mkContextFilterFn (fst <$> info) (mkContextFilter opt) False
        text' = ignoreCase opt . contextFilter $ text
        tokens = concatMap (runCompiledPosix text') compiled

    putMessageLnVerb 3 lock stderr $ "---\n" <> text' <> "\n---"
    putMessageLnVerb 1 lock stderr $ "strategy  : running regex (posix) search on " <> show filename
    putMessageLnVerb 2 lock stderr $ "tokens    : " <> show tokens

    if not (invert_match opt) && textOffsetWord8 text == 0
        then pure $! mkMatchesDirect filename text tokens
        else mkMatches (buildIndex text) filename text' tokens

runCompiledPosix :: T.Text -> CompiledPosixRegex -> [Chunk]
runCompiledPosix text'@(TI.Text arr baseOff _) CompiledPosixRegex{..} =
    let toChunks :: MatchText T.Text -> [Chunk]
        toChunks mt = [Chunk ChunkUnspec str | (str, _) <- elems mt]
     in case cprPrefilter of
            Nothing ->
                concatMap toChunks (matchAllText cprRegex text')
            Just disjuncts ->
                case findCandidateLines text' disjuncts of
                    [] -> []
                    linesList ->
                        concatMap
                            ( \(!ls, !le) ->
                                let !lineSlice = TI.Text arr (baseOff + ls) (le - ls)
                                 in concatMap toChunks (matchAllText cprRegex lineSlice)
                            )
                            linesList

findCandidateLines :: T.Text -> [[T.Text]] -> [(Int, Int)]
findCandidateLines text' disjuncts =
    case disjuncts of
        [conj] -> collectConjLines text' conj
        conjs -> foldl' (\acc c -> mergeUniqueLines acc (collectConjLines text' c)) [] conjs

collectConjLines :: T.Text -> [T.Text] -> [(Int, Int)]
collectConjLines _ [] = []
collectConjLines text'@(TI.Text arr@(A.ByteArray arr#) baseOff totalLen) conj =
    let !anchor = maximumBy (comparing T.length) conj
        others = delete anchor conj
        go :: Int -> [Int] -> [(Int, Int)]
        go !_ [] = []
        go !nextRel (rel : rest)
            | rel < nextRel = go nextRel rest
            | otherwise =
                let !ls = findLineStart arr baseOff (baseOff + rel - 1)
                    !le =
                        case memchr arr# (baseOff + rel) (totalLen - rel) (0x0A :: Word8) of
                            -1 -> totalLen
                            d -> rel + d
                    !nextRel' = le + 1
                    !lineSlice = TI.Text arr (baseOff + ls) (le - ls)
                 in if null others || all (\lit -> isJust (firstIndex lit lineSlice)) others
                        then (ls, le) : go nextRel' rest
                        else go nextRel' rest
     in go 0 (indicesFast anchor text')
  where
    findLineStart :: A.Array -> Int -> Int -> Int
    findLineStart !a !bOff !ix
        | ix < bOff = 0
        | A.unsafeIndex a ix == 0x0A = (ix + 1) - bOff
        | otherwise = findLineStart a bOff (ix - 1)

mergeUniqueLines :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
mergeUniqueLines [] ys = ys
mergeUniqueLines xs [] = xs
mergeUniqueLines xall@(x@(ls1, _) : xs) yall@(y@(ls2, _) : ys)
    | ls1 < ls2 = x : mergeUniqueLines xs yall
    | ls1 > ls2 = y : mergeUniqueLines xall ys
    | otherwise = x : mergeUniqueLines xs ys

-- ---------------------------------------------------------------------------
-- Sound POSIX ERE required-literal extractor for per-line pre-filtering
-- ---------------------------------------------------------------------------

type ReqLits = Maybe [[T.Text]]

maxDisjuncts :: Int
maxDisjuncts = 8

data Piece
    = PieceLit !Char
    | PieceReq !ReqLits

-- | Extracts a DNF of required literal substrings @[[T.Text]]@ from a POSIX ERE @pat@
-- iff:
--   1. No atom in @pat@ can consume @'\n'@ or depend on buffer-only anchors (@\`@, @\'@).
--   2. Every valid match of @pat@ is guaranteed to contain all literals in at least
--      one conjunction of the returned DNF.
-- Returns @Nothing@ whenever @pat@ may span lines, match without fixed literals, or use
-- unsupported/malformed syntax (falling back to full-text TDFA).
extractPosixLinePrefilter :: T.Text -> Maybe [[T.Text]]
extractPosixLinePrefilter pat
    | T.null pat || T.elem '\n' pat = Nothing
    | otherwise =
        case parseAlt (T.unpack pat) of
            Just (Just disjs, [])
                | not (null disjs) && all (\c -> not (null c) && all (not . T.null) c) disjs ->
                    Just disjs
            _ -> Nothing
  where
    parseAlt :: [Char] -> Maybe (ReqLits, [Char])
    parseAlt cs0 = do
        (r1, cs1) <- parseBranch cs0
        case cs1 of
            ('|' : cs2) -> do
                (r2, cs3) <- parseAlt cs2
                pure (altReq r1 r2, cs3)
            _ -> pure (r1, cs1)

    parseBranch :: [Char] -> Maybe (ReqLits, [Char])
    parseBranch cs0 = do
        (pieces, cs1) <- goPieces [] cs0
        let reqs = coalescePieces (reverse pieces)
        pure (foldl' seqReq Nothing reqs, cs1)

    goPieces :: [Piece] -> [Char] -> Maybe ([Piece], [Char])
    goPieces acc [] = Just (acc, [])
    goPieces acc cs@(c : _)
        | c == '|' || c == ')' = Just (acc, cs)
    goPieces _ (c : _)
        | c == '*' || c == '+' || c == '?' || c == '{' = Nothing
    goPieces acc cs = do
        (prim, cs1) <- parseAtom cs
        (piece, cs2) <- parsePostfixes prim cs1
        goPieces (piece : acc) cs2

    parseAtom :: [Char] -> Maybe (Either Char ReqLits, [Char])
    parseAtom [] = Nothing
    parseAtom ('(' : cs) = do
        (innerReq, cs1) <- parseAlt cs
        case cs1 of
            (')' : cs2) -> Just (Right innerReq, cs2)
            _ -> Nothing
    parseAtom ('[' : cs) = do
        csAfter <- parseBracketExpr cs
        Just (Right Nothing, csAfter)
    parseAtom ('\\' : c : cs)
        | c `elem` (".^$*+?()[]{}\\|" :: [Char]) = Just (Left c, cs)
        | c == 't' = Just (Left '\t', cs)
        | c == 'r' = Just (Left '\r', cs)
        | c `elem` ("wdb<>" :: [Char]) = Just (Right Nothing, cs)
        | otherwise = Nothing
    parseAtom ['\\'] = Nothing
    parseAtom ('.' : cs) = Just (Right Nothing, cs)
    parseAtom ('^' : cs) = Just (Right Nothing, cs)
    parseAtom ('$' : cs) = Just (Right Nothing, cs)
    parseAtom ('}' : _) = Nothing
    parseAtom (c : cs) = Just (Left c, cs)

    -- Positive POSIX bracket expression (rejecting `[^...]`, backslashes inside `[...]`,
    -- and character classes `[:space:]` / `[:cntrl:]` that include `'\n'`).
    parseBracketExpr :: [Char] -> Maybe [Char]
    parseBracketExpr [] = Nothing
    parseBracketExpr ('^' : _) = Nothing
    parseBracketExpr (']' : cs) = scanBracketBody cs
    parseBracketExpr cs = scanBracketBody cs

    scanBracketBody :: [Char] -> Maybe [Char]
    scanBracketBody [] = Nothing
    scanBracketBody ('\\' : _) = Nothing
    scanBracketBody (']' : cs) = Just cs
    scanBracketBody ('[' : ':' : cs) = do
        (clsName, afterCls) <- takeUntilColonClose [] cs
        if clsName == "space" || clsName == "cntrl"
            then Nothing
            else scanBracketBody afterCls
    scanBracketBody ('[' : '.' : cs) = do
        afterCol <- skipUntilClose '.' cs
        scanBracketBody afterCol
    scanBracketBody ('[' : '=' : cs) = do
        afterEq <- skipUntilClose '=' cs
        scanBracketBody afterEq
    scanBracketBody (_ : cs) = scanBracketBody cs

    takeUntilColonClose :: [Char] -> [Char] -> Maybe ([Char], [Char])
    takeUntilColonClose _ [] = Nothing
    takeUntilColonClose acc (':' : ']' : rest) = Just (reverse acc, rest)
    takeUntilColonClose acc (c : rest) = takeUntilColonClose (c : acc) rest

    skipUntilClose :: Char -> [Char] -> Maybe [Char]
    skipUntilClose _ [] = Nothing
    skipUntilClose delim (c : ']' : rest) | c == delim = Just rest
    skipUntilClose delim (_ : rest) = skipUntilClose delim rest

    parsePostfixes :: Either Char ReqLits -> [Char] -> Maybe (Piece, [Char])
    parsePostfixes (Left ch) cs@(q : _)
        | q == '*' || q == '+' || q == '?' || q == '{' =
            let baseReq = Just [[T.singleton ch]]
             in applyQuantLoop baseReq cs
    parsePostfixes (Left ch) cs = Just (PieceLit ch, cs)
    parsePostfixes (Right req) cs = applyQuantLoop req cs

    applyQuantLoop :: ReqLits -> [Char] -> Maybe (Piece, [Char])
    applyQuantLoop _ ('*' : cs) = applyQuantLoop Nothing cs
    applyQuantLoop _ ('?' : cs) = applyQuantLoop Nothing cs
    applyQuantLoop curReq ('+' : cs) = applyQuantLoop curReq cs
    applyQuantLoop curReq ('{' : cs) = do
        (minRep, afterBrace) <- parseBound cs
        let nextReq = if minRep >= 1 then curReq else Nothing
        applyQuantLoop nextReq afterBrace
    applyQuantLoop curReq cs = Just (PieceReq curReq, cs)

    parseBound :: [Char] -> Maybe (Int, [Char])
    parseBound cs0 = do
        (minDigits, cs1) <- takeDigits cs0
        let minVal = foldl' (\acc d -> acc * 10 + (fromEnum d - fromEnum '0')) 0 minDigits
        case cs1 of
            ('}' : rest) -> Just (minVal, rest)
            (',' : '}' : rest) -> Just (minVal, rest)
            (',' : cs2) -> do
                (_, cs3) <- takeDigits cs2
                case cs3 of
                    ('}' : rest) -> Just (minVal, rest)
                    _ -> Nothing
            _ -> Nothing

    takeDigits :: [Char] -> Maybe ([Char], [Char])
    takeDigits cs =
        let (ds, rest) = span (\c -> c >= '0' && c <= '9') cs
         in if null ds then Nothing else Just (ds, rest)

    coalescePieces :: [Piece] -> [ReqLits]
    coalescePieces [] = []
    coalescePieces (PieceLit c : rest) =
        let (moreChars, remPieces) = spanLit rest
            !txt = T.pack (c : moreChars)
         in Just [[txt]] : coalescePieces remPieces
    coalescePieces (PieceReq r : rest) = r : coalescePieces rest

    spanLit :: [Piece] -> ([Char], [Piece])
    spanLit (PieceLit c : rs) =
        let (cs, remP) = spanLit rs
         in (c : cs, remP)
    spanLit rs = ([], rs)

    altReq :: ReqLits -> ReqLits -> ReqLits
    altReq (Just d1) (Just d2)
        | length d1 + length d2 <= maxDisjuncts = Just (d1 ++ d2)
    altReq _ _ = Nothing

    seqReq :: ReqLits -> ReqLits -> ReqLits
    seqReq Nothing r2 = r2
    seqReq r1 Nothing = r1
    seqReq (Just d1) (Just d2)
        | length d1 * length d2 <= maxDisjuncts =
            Just [trimConj (c1 ++ c2) | c1 <- d1, c2 <- d2]
        | otherwise =
            if minLitLen d1 >= minLitLen d2 then Just d1 else Just d2

    trimConj :: [T.Text] -> [T.Text]
    trimConj xs
        | length xs <= 4 = xs
        | otherwise = take 4 (reverse (map snd ( foldl' insertSorted [] [(T.length t, t) | t <- xs] )))
      where
        insertSorted [] p = [p]
        insertSorted (q : qs) p
            | fst p <= fst q = p : q : qs
            | otherwise = q : insertSorted qs p

    minLitLen :: [[T.Text]] -> Int
    minLitLen [] = 0
    minLitLen ds = minimum [maximum (0 : map T.length c) | c <- ds]

-- ---------------------------------------------------------------------------
-- Reference implementation retained for differential QuickCheck & corpus tests
-- ---------------------------------------------------------------------------

searchRef :: MVar () -> Maybe (FileType, FileTypeInfo) -> OsPath -> T.Text -> [T.Text] -> Bool -> ReaderIO [Match]
searchRef lock info filename text patterns _strict = do
    Env{..} <- ask
    let lindex = buildIndex text
    let !contextFilter = mkContextFilterFn (fst <$> info) (mkContextFilter opt) False
    let text' = ignoreCase opt . contextFilter $ text
#ifdef ENABLE_PCRE
        (=~~~) = if regex_pcre opt then (Text.Regex.PCRE.=~) else (TDFA.=~)
#else
        (=~~~) = (TDFA.=~)
#endif
        tokens =
            map (\(str, (_, _)) -> Chunk ChunkUnspec str) $
                concatMap elems $
                    patterns >>= (\p -> elems (getAllTextMatches $ text' =~~~ p :: (Array Int) (MatchText T.Text)))
    putMessageLnVerb 3 lock stderr $ "---\n" <> text' <> "\n---"
    mkMatches lindex filename text' tokens

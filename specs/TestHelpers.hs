module TestHelpers (
    allTokensFilter,
    lookupFTInfo,
    filterSource,
    lexSource,
    runSearchMem,
    runSearchFile,
    matchLineNums,
    matchLines,
    matchChunkTexts,
) where

import CGrep.Common (getTargetContents, getTargetName)
import CGrep.FileType (FileType)
import CGrep.FileTypeMap (FileTypeInfo, FileTypeInfoMap (..))
import CGrep.FileTypeMapTH (fileTypeInfoLookup, fileTypeInfoMap, mkContextFilterFn)
import CGrep.Match (Match (..))
import CGrep.Parser.Chunk (Chunk (..))
import CGrep.Parser.Token (Token, TokenFilter (..), parseTokens)
import CGrep.Search (getSearcher, normalizePatterns)
import CGrep.Semantic.ContextFilter (mkContextFilter)
import Config (defaultConfig)
import Control.Concurrent.MVar (newMVar)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Text as T
import Options (Options (..))
import Reader (Env (..))
import System.OsPath (unsafeEncodeUtf)

allTokensFilter :: TokenFilter
allTokensFilter =
    TokenFilter
        { tfIdentifier = True
        , tfKeyword = True
        , tfNativeType = True
        , tfString = True
        , tfNumber = True
        , tfOperator = True
        , tfBracket = True
        }

lookupFTInfo :: FileType -> Maybe (FileType, FileTypeInfo)
lookupFTInfo ft = (ft,) <$> Map.lookup ft (unMapInfo fileTypeInfoMap)

filterSource :: FileType -> Options -> Bool -> T.Text -> T.Text
filterSource ft opt useMarkers =
    mkContextFilterFn (Just ft) (mkContextFilter opt) useMarkers

lexSource :: FileType -> Options -> Bool -> T.Text -> [Token]
lexSource ft opt isStrict txt =
    let info = snd <$> lookupFTInfo ft
        filtered = filterSource ft opt True txt
     in toList $ parseTokens allTokensFilter info isStrict filtered

runSearchMem :: Options -> FileType -> FilePath -> T.Text -> [T.Text] -> IO [Match]
runSearchMem opt ft fname txt rawPatterns = do
    lock <- newMVar ()
    let env = Env defaultConfig opt
        osp = unsafeEncodeUtf fname
        info = lookupFTInfo ft
        patterns = normalizePatterns opt rawPatterns
        searcher = getSearcher env
    runReaderT (searcher lock info osp txt patterns (strict opt)) env

runSearchFile :: Options -> FilePath -> [T.Text] -> IO [Match]
runSearchFile opt fp rawPatterns = do
    lock <- newMVar ()
    let env = Env defaultConfig opt
        osp = unsafeEncodeUtf fp
    txt <- getTargetContents osp
    let info = fileTypeInfoLookup opt osp
        patterns = normalizePatterns opt rawPatterns
        searcher = getSearcher env
    runReaderT (searcher lock info (getTargetName osp) txt patterns (strict opt)) env

matchLineNums :: [Match] -> [Int]
matchLineNums = map mLineNumb

matchLines :: [Match] -> [T.Text]
matchLines = map mLine

matchChunkTexts :: [Match] -> [[T.Text]]
matchChunkTexts = map (map cToken . mChunks)

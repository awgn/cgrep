module Paths_cgrep (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [6,4,12], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/root/.cabal/bin"
libdir     = "/root/.cabal/lib/x86_64-linux-ghc-7.8.4/cgrep-6.4.12"
datadir    = "/root/.cabal/share/x86_64-linux-ghc-7.8.4/cgrep-6.4.12"
libexecdir = "/root/.cabal/libexec"
sysconfdir = "/root/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cgrep_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cgrep_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cgrep_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cgrep_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cgrep_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

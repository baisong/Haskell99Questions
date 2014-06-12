module Paths_HaskellNoob (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/oren/.cabal/bin"
libdir     = "/home/oren/.cabal/lib/HaskellNoob-0.0.1/ghc-7.6.3"
datadir    = "/home/oren/.cabal/share/HaskellNoob-0.0.1"
libexecdir = "/home/oren/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "HaskellNoob_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HaskellNoob_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HaskellNoob_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskellNoob_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

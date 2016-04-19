module Paths_Assignment4 (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ugd/msimiste/Desktop/CPSC411/Assignment4/.cabal-sandbox/bin"
libdir     = "/home/ugd/msimiste/Desktop/CPSC411/Assignment4/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.2/Assignment4-0.1.0.0-EATf9vIL7qpHMR3QUs4Zn6"
datadir    = "/home/ugd/msimiste/Desktop/CPSC411/Assignment4/.cabal-sandbox/share/x86_64-linux-ghc-7.10.2/Assignment4-0.1.0.0"
libexecdir = "/home/ugd/msimiste/Desktop/CPSC411/Assignment4/.cabal-sandbox/libexec"
sysconfdir = "/home/ugd/msimiste/Desktop/CPSC411/Assignment4/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Assignment4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Assignment4_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Assignment4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Assignment4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Assignment4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

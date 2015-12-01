module Paths_HDBC_mysql (
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
version = Version [0,6,6,2] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/mirate/.cabal/bin"
libdir     = "/home/mirate/.cabal/lib/x86_64-linux-ghc-7.6.3/HDBC-mysql-0.6.6.2"
datadir    = "/home/mirate/.cabal/share/x86_64-linux-ghc-7.6.3/HDBC-mysql-0.6.6.2"
libexecdir = "/home/mirate/.cabal/libexec"
sysconfdir = "/home/mirate/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HDBC_mysql_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HDBC_mysql_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HDBC_mysql_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HDBC_mysql_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HDBC_mysql_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

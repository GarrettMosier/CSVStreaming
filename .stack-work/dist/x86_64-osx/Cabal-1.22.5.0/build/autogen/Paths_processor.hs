module Paths_processor (
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

bindir     = "/Users/x3g2/code/CSVStreaming/.stack-work/install/x86_64-osx/lts-5.10/7.10.3/bin"
libdir     = "/Users/x3g2/code/CSVStreaming/.stack-work/install/x86_64-osx/lts-5.10/7.10.3/lib/x86_64-osx-ghc-7.10.3/processor-0.1.0.0-00ID89D2HNx8SiljXlkKE9"
datadir    = "/Users/x3g2/code/CSVStreaming/.stack-work/install/x86_64-osx/lts-5.10/7.10.3/share/x86_64-osx-ghc-7.10.3/processor-0.1.0.0"
libexecdir = "/Users/x3g2/code/CSVStreaming/.stack-work/install/x86_64-osx/lts-5.10/7.10.3/libexec"
sysconfdir = "/Users/x3g2/code/CSVStreaming/.stack-work/install/x86_64-osx/lts-5.10/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "processor_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "processor_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "processor_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "processor_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "processor_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

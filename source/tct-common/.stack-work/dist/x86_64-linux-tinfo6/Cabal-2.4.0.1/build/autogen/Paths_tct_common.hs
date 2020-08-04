{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tct_common (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [3,3,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/bytekid/tools/tct-lctrs/.stack-work/install/x86_64-linux-tinfo6/9a2b58e7e95a684f72636911c647484f9754e38ef62f7dba81a41750c9b3c9d5/8.6.5/bin"
libdir     = "/home/bytekid/tools/tct-lctrs/.stack-work/install/x86_64-linux-tinfo6/9a2b58e7e95a684f72636911c647484f9754e38ef62f7dba81a41750c9b3c9d5/8.6.5/lib/x86_64-linux-ghc-8.6.5/tct-common-3.3.0-3XAP4S4EE2GBSi6uAprw9E"
dynlibdir  = "/home/bytekid/tools/tct-lctrs/.stack-work/install/x86_64-linux-tinfo6/9a2b58e7e95a684f72636911c647484f9754e38ef62f7dba81a41750c9b3c9d5/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/bytekid/tools/tct-lctrs/.stack-work/install/x86_64-linux-tinfo6/9a2b58e7e95a684f72636911c647484f9754e38ef62f7dba81a41750c9b3c9d5/8.6.5/share/x86_64-linux-ghc-8.6.5/tct-common-3.3.0"
libexecdir = "/home/bytekid/tools/tct-lctrs/.stack-work/install/x86_64-linux-tinfo6/9a2b58e7e95a684f72636911c647484f9754e38ef62f7dba81a41750c9b3c9d5/8.6.5/libexec/x86_64-linux-ghc-8.6.5/tct-common-3.3.0"
sysconfdir = "/home/bytekid/tools/tct-lctrs/.stack-work/install/x86_64-linux-tinfo6/9a2b58e7e95a684f72636911c647484f9754e38ef62f7dba81a41750c9b3c9d5/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tct_common_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tct_common_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tct_common_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tct_common_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tct_common_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tct_common_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

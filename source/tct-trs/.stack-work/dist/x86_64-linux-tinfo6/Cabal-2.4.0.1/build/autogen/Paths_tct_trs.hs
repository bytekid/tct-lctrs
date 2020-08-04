{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tct_trs (
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

bindir     = "/home/bytekid/tools/tct-lctrs/.stack-work/install/x86_64-linux-tinfo6/d278d82eda51f8c8323636fcf27e419c3dd15536604f018e2aa08915c047f945/8.6.5/bin"
libdir     = "/home/bytekid/tools/tct-lctrs/.stack-work/install/x86_64-linux-tinfo6/d278d82eda51f8c8323636fcf27e419c3dd15536604f018e2aa08915c047f945/8.6.5/lib/x86_64-linux-ghc-8.6.5/tct-trs-3.3.0-DBMBzayMoGt92AfMNLROe8"
dynlibdir  = "/home/bytekid/tools/tct-lctrs/.stack-work/install/x86_64-linux-tinfo6/d278d82eda51f8c8323636fcf27e419c3dd15536604f018e2aa08915c047f945/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/bytekid/tools/tct-lctrs/.stack-work/install/x86_64-linux-tinfo6/d278d82eda51f8c8323636fcf27e419c3dd15536604f018e2aa08915c047f945/8.6.5/share/x86_64-linux-ghc-8.6.5/tct-trs-3.3.0"
libexecdir = "/home/bytekid/tools/tct-lctrs/.stack-work/install/x86_64-linux-tinfo6/d278d82eda51f8c8323636fcf27e419c3dd15536604f018e2aa08915c047f945/8.6.5/libexec/x86_64-linux-ghc-8.6.5/tct-trs-3.3.0"
sysconfdir = "/home/bytekid/tools/tct-lctrs/.stack-work/install/x86_64-linux-tinfo6/d278d82eda51f8c8323636fcf27e419c3dd15536604f018e2aa08915c047f945/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tct_trs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tct_trs_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tct_trs_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tct_trs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tct_trs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tct_trs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

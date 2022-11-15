{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_IntegrationLibrary (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\milae\\Downloads\\IntegrationHaskellLibrary\\.stack-work\\install\\804b459e\\bin"
libdir     = "C:\\Users\\milae\\Downloads\\IntegrationHaskellLibrary\\.stack-work\\install\\804b459e\\lib\\x86_64-windows-ghc-9.0.2\\IntegrationLibrary-0.1.0.0-GbDhKcFusTx4GtHyKnhj2Z"
dynlibdir  = "C:\\Users\\milae\\Downloads\\IntegrationHaskellLibrary\\.stack-work\\install\\804b459e\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\milae\\Downloads\\IntegrationHaskellLibrary\\.stack-work\\install\\804b459e\\share\\x86_64-windows-ghc-9.0.2\\IntegrationLibrary-0.1.0.0"
libexecdir = "C:\\Users\\milae\\Downloads\\IntegrationHaskellLibrary\\.stack-work\\install\\804b459e\\libexec\\x86_64-windows-ghc-9.0.2\\IntegrationLibrary-0.1.0.0"
sysconfdir = "C:\\Users\\milae\\Downloads\\IntegrationHaskellLibrary\\.stack-work\\install\\804b459e\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "IntegrationLibrary_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "IntegrationLibrary_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "IntegrationLibrary_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "IntegrationLibrary_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "IntegrationLibrary_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "IntegrationLibrary_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_projeto_JIT_compiler (
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

bindir     = "/mnt/c/Users/ivanc/Desktop/UnB/Disciplinas/T\243picos - Edil/projeto-JIT-compiler/.stack-work/install/x86_64-linux-tinfo6/de467e15908489be3fd5440ece2557b7087ad65831fbef331fc99e813d18cf9f/8.10.4/bin"
libdir     = "/mnt/c/Users/ivanc/Desktop/UnB/Disciplinas/T\243picos - Edil/projeto-JIT-compiler/.stack-work/install/x86_64-linux-tinfo6/de467e15908489be3fd5440ece2557b7087ad65831fbef331fc99e813d18cf9f/8.10.4/lib/x86_64-linux-ghc-8.10.4/projeto-JIT-compiler-0.1.0.0-47E1iwP09IBKUIgCKhPpVt-JITCompHaskell-exe"
dynlibdir  = "/mnt/c/Users/ivanc/Desktop/UnB/Disciplinas/T\243picos - Edil/projeto-JIT-compiler/.stack-work/install/x86_64-linux-tinfo6/de467e15908489be3fd5440ece2557b7087ad65831fbef331fc99e813d18cf9f/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/mnt/c/Users/ivanc/Desktop/UnB/Disciplinas/T\243picos - Edil/projeto-JIT-compiler/.stack-work/install/x86_64-linux-tinfo6/de467e15908489be3fd5440ece2557b7087ad65831fbef331fc99e813d18cf9f/8.10.4/share/x86_64-linux-ghc-8.10.4/projeto-JIT-compiler-0.1.0.0"
libexecdir = "/mnt/c/Users/ivanc/Desktop/UnB/Disciplinas/T\243picos - Edil/projeto-JIT-compiler/.stack-work/install/x86_64-linux-tinfo6/de467e15908489be3fd5440ece2557b7087ad65831fbef331fc99e813d18cf9f/8.10.4/libexec/x86_64-linux-ghc-8.10.4/projeto-JIT-compiler-0.1.0.0"
sysconfdir = "/mnt/c/Users/ivanc/Desktop/UnB/Disciplinas/T\243picos - Edil/projeto-JIT-compiler/.stack-work/install/x86_64-linux-tinfo6/de467e15908489be3fd5440ece2557b7087ad65831fbef331fc99e813d18cf9f/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "projeto_JIT_compiler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "projeto_JIT_compiler_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "projeto_JIT_compiler_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "projeto_JIT_compiler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "projeto_JIT_compiler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "projeto_JIT_compiler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

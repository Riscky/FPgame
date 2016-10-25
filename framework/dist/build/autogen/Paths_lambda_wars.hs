module Paths_lambda_wars (
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

bindir     = "/home/rick/Projects/FP/A5/framework/.cabal-sandbox/bin"
libdir     = "/home/rick/Projects/FP/A5/framework/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/lambda-wars-0.1.0.0-IPYsAo2zTfhGvanOhwCqDi"
datadir    = "/home/rick/Projects/FP/A5/framework/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/lambda-wars-0.1.0.0"
libexecdir = "/home/rick/Projects/FP/A5/framework/.cabal-sandbox/libexec"
sysconfdir = "/home/rick/Projects/FP/A5/framework/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lambda_wars_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lambda_wars_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lambda_wars_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lambda_wars_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lambda_wars_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

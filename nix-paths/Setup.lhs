#! /usr/bin/env runhaskell

> module Main ( main ) where
>
> import Data.Char
> import Data.Maybe
> import Distribution.PackageDescription
> import Distribution.Simple
> import Distribution.Simple.LocalBuildInfo
> import Distribution.Simple.Program
> import Distribution.Simple.Setup
>
> main :: IO ()
> main = defaultMainWithHooks simpleUserHooks
>        { hookedPrograms = map simpleProgram programs
>        , confHook = configure
>        }
>
> programs :: [String]
> programs = ["nix-instantiate", "nix-build", "nix-env", "nix-store"]
>
> configure :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
> configure (gpd, hbi) flags = do
>   lbi' <- confHook simpleUserHooks (gpd, hbi) flags
>   let paths = map (\p -> (p, maybe p programPath (lookupProgram (simpleProgram p) (withPrograms lbi)))) programs
>       descr = localPkgDescr lbi'
>       lib = fromJust (library descr)
>       libbi = libBuildInfo lib
>       lbi = lbi' { localPkgDescr = descr
>                    { library = Just (lib { libBuildInfo = libbi { cppOptions = cppOptions libbi ++ map fmtCppArg paths } })
>                    }
>                  }
>   return lbi
>
> fmtCppArg :: (String, FilePath) -> String
> fmtCppArg (prg, path) = "-DPATH_TO_" ++ map mangle prg ++ "=" ++ show path
>   where
>     mangle :: Char -> Char
>     mangle '-' = '_'
>     mangle c = toUpper c

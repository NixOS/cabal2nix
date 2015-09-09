#! /usr/bin/env runhaskell

> module Main ( main ) where
>
> import Data.Char
> import Distribution.PackageDescription
> import Distribution.Simple
> import Distribution.Simple.BuildPaths
> import Distribution.Simple.LocalBuildInfo
> import Distribution.Simple.Program
> import Distribution.Simple.Setup
> import System.Directory ( createDirectoryIfMissing )
> import System.FilePath
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
>   lbi <- confHook simpleUserHooks (gpd, hbi) flags
>   let paths = [ (p, programPath pc) | p <- programs, Just pc <- [lookupProgram (simpleProgram p) (withPrograms lbi)] ]
>       dirPath = autogenModulesDir lbi </> "Nix"
>   createDirectoryIfMissing True dirPath
>   writeFile (dirPath </> "Paths.hs") $
>     unlines $
>      "module Nix.Paths where\n" : map (uncurry fmtPath) paths
>   return lbi
>
> fmtPath :: String -> FilePath -> String
> fmtPath name path = unlines
>   [ "-- | Complete path to the @" ++ name ++ "@ executable."
>   , mangle name ++ " :: FilePath"
>   , mangle name ++ " = " ++ show path
>   ]
>
> mangle :: String -> String
> mangle [] = []
> mangle ('-':c:cs) = toUpper c : mangle cs
> mangle (c:cs) = c : mangle cs

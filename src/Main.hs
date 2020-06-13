{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Main ( main ) where

import Control.Exception ( bracket )
import Control.Lens
import Control.Monad
import Data.List ( intercalate, isPrefixOf )
import Data.List.Split
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe, listToMaybe )
import qualified Data.Set as Set
import Data.String
import Distribution.Compiler
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Haskell.FromCabal.Flags
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import Distribution.Package ( packageId )
import Distribution.PackageDescription ( mkFlagName, mkFlagAssignment, FlagAssignment )
import Distribution.PackageDescription.Parsec
import Distribution.Parsec as P
import Distribution.Simple.Utils ( lowercase )
import Distribution.System
import Distribution.Verbosity ( silent )
import Language.Nix
import Options.Applicative
import Paths_cabal2nix_v3 ( version )
import System.Environment ( getArgs )
import System.IO ( hFlush, stdout, stderr )
import Text.PrettyPrint.HughesPJClass ( prettyShow )

{-# ANN module ("HLint: ignore Use Just" :: String) #-}

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() ->
         getArgs >>= cabal2nix

cabal2nix :: [String] -> IO ()
cabal2nix = parseArgs >=> cabal2nix' >=> putStrLn . prettyShow

cabal2nix' :: Options -> IO Derivation
cabal2nix' opts@Options {..} = do
  gpd <- readGenericPackageDescription silent optFilePath
  let flags :: FlagAssignment
      flags = configureCabalFlags (packageId gpd) `mappend` readFlagList optFlags

      deriv :: Derivation
      deriv = fromGenericPackageDescription (const True)
                                            (\i -> Just (binding # (i, path # [ident # "pkgs", i])))
                                            optSystem
                                            (unknownCompilerInfo optCompiler NoAbiTag)
                                            flags
                                            []
                                            gpd
              & src .~ DerivationSource "" "mirror://hackage/foobar" "" "" Nothing
              & extraFunctionArgs %~ Set.union (Set.fromList ("inherit stdenv":map (fromString . ("inherit " ++)) optExtraArgs))
              & extraAttributes %~ Map.union (readExtraAttributes optExtraAttributes)
  return deriv

parseArgs :: [String] -> IO Options
parseArgs = handleParseResult . execParserPure defaultPrefs pinfo

pinfo :: ParserInfo Options
pinfo = info
        (   helper
        <*> infoOption ("cabal2nix " ++ prettyShow version) (long "version" <> help "Show version number")
        <*> options
        )
        (  fullDesc
        <> header "cabal2nix converts Cabal files into build instructions for Nix."
        )

data Options = Options
  { optFlags :: [String]
  , optCompiler :: CompilerId
  , optSystem :: Platform
  , optExtraArgs :: [String]
  , optFilePath :: FilePath
  , optExtraAttributes :: [String]
  }
  deriving Show

options :: Parser Options
options = do
  optFlags <- many (strOption $ short 'f' <> long "flag" <> help "Cabal flag (may be specified multiple times)")
  optCompiler <- option parseCabal (long "compiler" <> help "compiler to use when evaluating the Cabal file" <> value buildCompilerId <> showDefaultWith prettyShow)
  optSystem <- option (maybeReader parsePlatform) (long "system" <> help "host system (in either short Nix format or full LLVM style) to use when evaluating the Cabal file" <> value buildPlatform <> showDefaultWith prettyShow)
  optExtraArgs <- many (strOption $ long "extra-arguments" <> help "extra parameters required for the function body")
  optFilePath <- strArgument (metavar "FILE")
  optExtraAttributes <- many (strArgument (metavar "EXTRA-ATTRIBUTES"))
  return (Options {..})

-- Utils

readExtraAttributes :: [String] -> Map String String
readExtraAttributes = Map.fromList . map parseKeyValue

parseKeyValue :: String -> (String, String)
parseKeyValue x = (k, drop 1 v')
  where (k,v') = break (=='=') x

readFlagList :: [String] -> FlagAssignment
readFlagList = mkFlagAssignment . map tagWithValue
  where tagWithValue ('-':fname) = (mkFlagName (lowercase fname), False)
        tagWithValue fname       = (mkFlagName (lowercase fname), True)

parseCabal :: Parsec a => ReadM a
parseCabal = eitherReader eitherParsec

-- | Replicate the normalization performed by GHC_CONVERT_CPU in GHC's aclocal.m4
-- since the output of that is what Cabal parses.
ghcConvertArch :: String -> String
ghcConvertArch arch = case arch of
  "i486"  -> "i386"
  "i586"  -> "i386"
  "i686"  -> "i386"
  "amd64" -> "x86_64"
  _ -> fromMaybe arch $ listToMaybe
    [prefix | prefix <- archPrefixes, prefix `isPrefixOf` arch]
  where archPrefixes =
          [ "aarch64", "alpha", "arm", "hppa1_1", "hppa", "m68k", "mipseb"
          , "mipsel", "mips", "powerpc64le", "powerpc64", "powerpc", "s390x"
          , "sparc64", "sparc"
          ]

-- | Replicate the normalization performed by GHC_CONVERT_OS in GHC's aclocal.m4
-- since the output of that is what Cabal parses.
ghcConvertOS :: String -> String
ghcConvertOS os = case os of
  "watchos"       -> "ios"
  "tvos"          -> "ios"
  "linux-android" -> "linux-android"
  "linux-androideabi" -> "linux-androideabi"
  _ | "linux-" `isPrefixOf` os -> "linux"
  _ -> fromMaybe os $ listToMaybe
    [prefix | prefix <- osPrefixes, prefix `isPrefixOf` os]
  where osPrefixes =
          [ "gnu", "openbsd", "aix", "darwin", "solaris2", "freebsd", "nto-qnx"]

parseArch :: String -> Arch
parseArch = classifyArch Permissive . ghcConvertArch

parseOS :: String -> OS
parseOS = classifyOS Permissive . ghcConvertOS

parsePlatform :: String -> Maybe Platform
parsePlatform = parsePlatformParts . splitOn "-"

parsePlatformParts :: [String] -> Maybe Platform
parsePlatformParts = \case
  [arch, os] ->
    Just $ Platform (parseArch arch) (parseOS os)
  (arch : _ : osParts) ->
    Just $ Platform (parseArch arch) $ parseOS $ intercalate "-" osParts
  _ -> Nothing

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Cabal2nix
  ( main, cabal2nix, cabal2nix', cabal2nixWithDB, parseArgs
  , Options(..)
  )
  where

import Control.Exception ( bracket )
import Control.Lens
import Control.Monad
import Data.List ( intercalate, isPrefixOf )
import Data.List.Split
import Data.Maybe ( fromMaybe, isJust, listToMaybe )
import qualified Data.Set as Set
import Data.String
import Data.Time
import Distribution.Compiler
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Haskell.FromCabal.Flags
import qualified Distribution.Nixpkgs.Haskell.FromCabal.PostProcess as PP (pkg)
import qualified Distribution.Nixpkgs.Haskell.Hackage as DB
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import Distribution.Nixpkgs.Haskell.PackageSourceSpec
import Distribution.Nixpkgs.Meta
import Distribution.Package ( packageId )
import Distribution.PackageDescription ( mkFlagName, mkFlagAssignment, FlagAssignment )
import Distribution.Parsec as P
import Distribution.Simple.Utils ( lowercase )
import Distribution.System
import Language.Nix
import Options.Applicative
import Paths_cabal2nix ( version )
import System.Environment ( getArgs )
import System.IO ( hFlush, hPutStrLn, stdout, stderr )
import qualified Text.PrettyPrint.ANSI.Leijen as P2
import Text.PrettyPrint.HughesPJClass ( Doc, Pretty(..), text, vcat, hcat, semi, render, prettyShow )

{-# ANN module ("HLint: ignore Use Just" :: String) #-}

data Options = Options
  { optSha256 :: Maybe String
  , optMaintainer :: [String]
--, optPlatform :: [String]       -- TODO: fix command line handling of platforms
  , optHaddock :: Bool
  , optHpack :: HpackUse
  , optDoCheck :: Bool
  , optJailbreak :: Bool
  , optDoBenchmark :: Bool
  , optRevision :: Maybe String
  , optHyperlinkSource :: Bool
  , optEnableLibraryProfiling :: Bool
  , optEnableExecutableProfiling :: Bool
  , optEnableProfiling :: Maybe Bool
  , optExtraArgs :: [String]
  , optHackageDb :: Maybe FilePath
  , optNixShellOutput :: Bool
  , optFlags :: [String]
  , optCompiler :: CompilerId
  , optSystem :: Platform
  , optSubpath :: Maybe FilePath
  , optHackageSnapshot :: Maybe UTCTime
  , optNixpkgsIdentifier :: NixpkgsResolver
  , optUrl :: String
  , optFetchSubmodules :: FetchSubmodules
  }

options :: Parser Options
options = do
  optSha256
    <- optional (strOption $ long "sha256" <> metavar "HASH" <> help "sha256 hash of source tarball")
  optMaintainer
    <- many (strOption $ long "maintainer" <> metavar "MAINTAINER" <> help "maintainer of this package (may be specified multiple times)")
-- optPlatform <- many (strOption $ long "platform" <> metavar "PLATFORM" <> help "supported build platforms (may be specified multiple times)")
  optHaddock
    <- flag True False (long "no-haddock" <> help "don't run Haddock when building this package")
  optHpack
    <-
      (
        flag' ForceHpack (long "hpack" <> help "run hpack before configuring this package (only non-hackage packages)")
        <|>
        flag' NoHpack (long "no-hpack" <> help "disable hpack run and use only cabal disregarding package.yaml existence")
        <|>
        pure PackageYamlHpack
      )
  optDoCheck
    <- flag True False (long "no-check" <> help "don't run regression test suites of this package")
  optJailbreak
    <- switch (long "jailbreak" <> help "disregard version restrictions on build inputs")
  optDoBenchmark
    <- switch (long "benchmark" <> help "enable benchmarks for this package")
  optRevision
    <- optional (strOption $ long "revision" <> help "revision to use when fetching from VCS")
  optHyperlinkSource
    <- flag True False (long "no-hyperlink-source" <> help "don't generate pretty-printed source code for the documentation")
  optEnableLibraryProfiling
    <- switch (long "enable-library-profiling" <> help "enable library profiling in the generated build")
  optEnableExecutableProfiling
    <- switch (long "enable-executable-profiling" <> help "enable executable profiling in the generated build")
  optEnableProfiling
    <- optional (switch (long "enable-profiling" <> help "enable both library and executable profiling in the generated build"))
  optExtraArgs
    <- many (strOption $ long "extra-arguments" <> help "extra parameters required for the function body")
  optHackageDb
    <- optional (strOption $ long "hackage-db" <> metavar "PATH" <> help "path to the local hackage db in tar format")
  optNixShellOutput
    <- switch (long "shell" <> help "generate output suitable for nix-shell")
  optFlags
    <- many (strOption $ short 'f' <> long "flag" <> help "Cabal flag (may be specified multiple times)")
  optCompiler
    <- option parseCabal (long "compiler" <> help "compiler to use when evaluating the Cabal file" <> value buildCompilerId <> showDefaultWith prettyShow)
  optSystem
    <- option (maybeReader parsePlatform) (long "system" <> help "host system (in either short Nix format or full LLVM style) to use when evaluating the Cabal file" <> value buildPlatform <> showDefaultWith prettyShow)
  optSubpath
    <- optional (strOption $ long "subpath" <> metavar "PATH" <> help "Path to Cabal file's directory relative to the URI (default is root directory)")
  optHackageSnapshot
    <- optional (option utcTimeReader (long "hackage-snapshot" <> help "hackage snapshot time, ISO format"))
  optNixpkgsIdentifier
    <- pure (\i -> Just (binding # (i, path # [ident # "pkgs", i])))
  optUrl
    <- strArgument (metavar "URI")
  optFetchSubmodules
    <- flag FetchSubmodules DontFetchSubmodules  (long "dont-fetch-submodules" <> help "do not fetch git submodules from git sources")
  pure Options{..}

-- | A parser for the date. Hackage updates happen maybe once or twice a month.
-- Example: parseTime defaultTimeLocale "%FT%T%QZ" "2017-11-20T12:18:35Z" :: Maybe UTCTime
utcTimeReader :: ReadM UTCTime
utcTimeReader = eitherReader $ \arg ->
    case parseTimeM True defaultTimeLocale "%FT%T%QZ" arg of
        Nothing      -> Left $ "Cannot parse date, ISO format used ('2017-11-20T12:18:35Z'): " ++ arg
        Just utcTime -> Right utcTime

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

pinfo :: ParserInfo Options
pinfo = info
        (   helper
        <*> infoOption ("cabal2nix " ++ prettyShow version) (long "version" <> help "Show version number")
        <*> options
        )
        (  fullDesc
        <> header "cabal2nix converts Cabal files into build instructions for Nix."
        <> progDescDoc (Just (P2.vcat
                     [ P2.text ""
                     , P2.text "Recognized URI schemes:"
                     , P2.text ""
                     , P2.text "  cabal://pkgname-pkgversion     download the specified package from Hackage"
                     , P2.text "  cabal://pkgname                download latest version of this package from Hackage"
                     , P2.text "  file:///local/path             load the Cabal file from the local disk"
                     , P2.text "  /local/path                    abbreviated version of file URI"
                     , P2.text "  <git/svn/bzr/hg URL>           download the source from the specified repository"
                     , P2.text ""
                     , P2.fillSep (map P2.text (words (  "If the URI refers to a cabal file, information for building the package "
                                                      ++ "will be retrieved from that file, but hackage will be used as a source "
                                                      ++ "for the derivation. Otherwise, the supplied URI will be used to as the "
                                                      ++ "source for the derivation and the information is taken from the cabal file "
                                                      ++ "at the root of the downloaded source."
                                                      )))
                     ]))
        )

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() ->
  cabal2nix =<< getArgs

hpackOverrides :: Derivation -> Derivation
hpackOverrides = over phaseOverrides (++ "prePatch = \"hpack\";")
               . set (libraryDepends . tool . contains (PP.pkg "hpack")) True

cabal2nix' :: Options -> IO (Either Doc Derivation)
cabal2nix' opts@Options{..} = do
  pkg <- getPackage optHpack optFetchSubmodules optHackageDb optHackageSnapshot $
         Source {
           sourceUrl = optUrl,
           sourceRevision = fromMaybe "" optRevision,
           sourceHash = case optSha256 of
             Nothing -> UnknownHash
             Just hash -> Guess hash,
           sourceCabalDir = fromMaybe "" optSubpath
         }
  processPackage opts pkg

cabal2nixWithDB :: DB.HackageDB -> Options -> IO (Either Doc Derivation)
cabal2nixWithDB db opts@Options{..} = do
  when (isJust optHackageDb) $ hPutStrLn stderr "WARN: HackageDB provided directly; ignoring --hackage-db"
  when (isJust optHackageSnapshot) $ hPutStrLn stderr "WARN: HackageDB provided directly; ignoring --hackage-snapshot"
  pkg <- getPackage' optHpack optFetchSubmodules (return db) $
         Source {
           sourceUrl = optUrl,
           sourceRevision = fromMaybe "" optRevision,
           sourceHash = case optSha256 of
             Nothing -> UnknownHash
             Just hash -> Guess hash,
           sourceCabalDir = fromMaybe "" optSubpath
         }
  processPackage opts pkg

processPackage :: Options -> Package -> IO (Either Doc Derivation)
processPackage Options{..} pkg = do
  let
      withHpackOverrides :: Derivation -> Derivation
      withHpackOverrides = if pkgRanHpack pkg then hpackOverrides else id

      flags :: FlagAssignment
      flags = configureCabalFlags (packageId (pkgCabal pkg)) `mappend` readFlagList optFlags

      deriv :: Derivation
      deriv = withHpackOverrides $ fromGenericPackageDescription (const True)
                                            optNixpkgsIdentifier
                                            optSystem
                                            (unknownCompilerInfo optCompiler NoAbiTag)
                                            flags
                                            []
                                            (pkgCabal pkg)
              & src .~ pkgSource pkg
              & subpath .~ fromMaybe "." optSubpath
              & runHaddock %~ (optHaddock &&)
              & jailbreak .~ optJailbreak
              & hyperlinkSource .~ optHyperlinkSource
              & enableLibraryProfiling .~ (fromMaybe False optEnableProfiling || optEnableLibraryProfiling)
              & enableExecutableProfiling .~ (fromMaybe False optEnableProfiling || optEnableExecutableProfiling)
              & metaSection.maintainers .~ Set.fromList (map (review ident) optMaintainer)
--            & metaSection.platforms .~ Set.fromList optPlatform
              & doCheck &&~ optDoCheck
              & doBenchmark ||~ optDoBenchmark
              & extraFunctionArgs %~ Set.union (Set.fromList ("inherit lib":map (fromString . ("inherit " ++)) optExtraArgs))

      shell :: Doc
      shell = vcat
              [ text "{ nixpkgs ? import <nixpkgs> {}, compiler ? \"default\", doBenchmark ? false }:"
              , text ""
              , text "let"
              , text ""
              , text "  inherit (nixpkgs) pkgs;"
              , text ""
              , hcat [ text "  f = ", pPrint deriv, semi ]
              , text ""
              , text "  haskellPackages = if compiler == \"default\""
              , text "                       then pkgs.haskellPackages"
              , text "                       else pkgs.haskell.packages.${compiler};"
              , text ""
              , text "  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;"
              , text ""
              , text "  drv = variant (haskellPackages.callPackage f {});"
              , text ""
              , text "in"
              , text ""
              , text "  if pkgs.lib.inNixShell then drv.env else drv"
              ]
  pure $ if optNixShellOutput then Left shell else Right deriv

cabal2nix :: [String] -> IO ()
cabal2nix = parseArgs >=> cabal2nix' >=> putStrLn . either render prettyShow

parseArgs :: [String] -> IO Options
parseArgs = handleParseResult . execParserPure defaultPrefs pinfo

-- Utils

readFlagList :: [String] -> FlagAssignment
readFlagList = mkFlagAssignment . map tagWithValue
  where tagWithValue ('-':fname) = (mkFlagName (lowercase fname), False)
        tagWithValue fname       = (mkFlagName (lowercase fname), True)

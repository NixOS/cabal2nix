{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Cabal2nix
  ( main, cabal2nix, cabal2nix'
  )
  where

import Control.Exception ( bracket )
import Control.Lens
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )
import qualified Data.Set as Set
import Data.String
import qualified Distribution.Compat.ReadP as P
import Distribution.Compiler
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Haskell.FromCabal
import qualified Distribution.Nixpkgs.Haskell.FromCabal.PostProcess as PP (pkg)
import Distribution.Nixpkgs.Haskell.PackageSourceSpec
import Distribution.Nixpkgs.Meta
import Distribution.PackageDescription ( mkFlagName, FlagAssignment )
import Distribution.Simple.Utils ( lowercase )
import Distribution.System
import Distribution.Text
import Language.Nix
import Options.Applicative
import Paths_cabal2nix ( version )
import System.Environment ( getArgs )
import System.IO ( hFlush, stdout, stderr )
import qualified Text.PrettyPrint.ANSI.Leijen as P2 hiding ( (<$>), (<>) )
import Text.PrettyPrint.HughesPJClass ( Doc, Pretty(..), text, vcat, hcat, semi )

data Options = Options
  { optSha256 :: Maybe String
  , optMaintainer :: [String]
--, optPlatform :: [String]       -- TODO: fix command line handling of platforms
  , optHaddock :: Bool
  , optHpack :: Bool
  , optDoCheck :: Bool
  , optJailbreak :: Bool
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
  , optUrl :: String
  }
  deriving (Show)

options :: Parser Options
options = Options
          <$> optional (strOption $ long "sha256" <> metavar "HASH" <> help "sha256 hash of source tarball")
          <*> many (strOption $ long "maintainer" <> metavar "MAINTAINER" <> help "maintainer of this package (may be specified multiple times)")
--        <*> many (strOption $ long "platform" <> metavar "PLATFORM" <> help "supported build platforms (may be specified multiple times)")
          <*> flag True False (long "no-haddock" <> help "don't run Haddock when building this package")
          <*> switch (long "hpack" <> help "run hpack before configuring this package (only non-hackage packages)")
          <*> flag True False (long "no-check" <> help "don't run regression test suites of this package")
          <*> switch (long "jailbreak" <> help "disregard version restrictions on build inputs")
          <*> optional (strOption $ long "revision" <> help "revision to use when fetching from VCS")
          <*> flag True False (long "no-hyperlink-source" <> help "don't generate pretty-printed source code for the documentation")
          <*> switch (long "enable-library-profiling" <> help "enable library profiling in the generated build")
          <*> switch (long "enable-executable-profiling" <> help "enable executable profiling in the generated build")
          <*> optional (switch (long "enable-profiling" <> help "enable both library and executable profiling in the generated build"))
          <*> many (strOption $ long "extra-arguments" <> help "extra parameters required for the function body")
          <*> optional (strOption $ long "hackage-db" <> metavar "PATH" <> help "path to the local hackage db in tar format")
          <*> switch (long "shell" <> help "generate output suitable for nix-shell")
          <*> many (strOption $ short 'f' <> long "flag" <> help "Cabal flag (may be specified multiple times)")
          <*> option (readP parse) (long "compiler" <> help "compiler to use when evaluating the Cabal file" <> value buildCompilerId <> showDefaultWith display)
          <*> option (readP parsePlatform) (long "system" <> help "target system to use when evaluating the Cabal file" <> value buildPlatform <> showDefaultWith display)
          <*> optional (strOption $ long "subpath" <> metavar "PATH" <> help "Path to Cabal file's directory relative to the URI (default is root directory)")
          <*> strArgument (metavar "URI")

readP :: P.ReadP a a -> ReadM a
readP p = eitherReader $ \s -> case [ r' | (r',"") <- P.readP_to_S p s ] of
                                    (r:_) -> Right r
                                    _     -> Left ("invalid value " ++ show s)

parsePlatform :: P.ReadP r Platform
parsePlatform = do arch <- P.choice [P.string "i686" >> return I386, P.string "x86_64" >> return X86_64]
                   _ <- P.char '-'
                   os <- P.choice [P.string "linux" >> return Linux, P.string "darwin" >> return OSX]
                   return (Platform arch os)

pinfo :: ParserInfo Options
pinfo = info
        (   helper
        <*> infoOption ("cabal2nix " ++ display version) (long "version" <> help "Show version number")
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
hpackOverrides = over phaseOverrides (++ "preConfigure = \"${hpack}/bin/hpack;\";")
               . set (libraryDepends . pkgconfig . contains (PP.pkg "hpack")) True

cabal2nix' :: [String] -> IO (Either Doc Derivation)
cabal2nix' args = do
  Options {..} <- handleParseResult $ execParserPure defaultPrefs pinfo args

  pkg <- getPackage optHpack optHackageDb $ Source optUrl (fromMaybe "" optRevision) (maybe UnknownHash Guess optSha256) (fromMaybe "" optSubpath)

  let
      withHpackOverrides :: Derivation -> Derivation
      withHpackOverrides = if pkgRanHpack pkg then hpackOverrides else id
      deriv :: Derivation
      deriv = withHpackOverrides $ fromGenericPackageDescription (const True)
                                            (\i -> Just (binding # (i, path # [i])))
                                            optSystem
                                            (unknownCompilerInfo optCompiler NoAbiTag)
                                            (readFlagList optFlags)
                                            []
                                            (pkgCabal pkg)
              & src .~ pkgSource pkg
              & subpath .~ (fromMaybe "." optSubpath)
              & runHaddock .~ optHaddock
              & jailbreak .~ optJailbreak
              & hyperlinkSource .~ optHyperlinkSource
              & enableLibraryProfiling .~ (fromMaybe False optEnableProfiling || optEnableLibraryProfiling)
              & enableExecutableProfiling .~ (fromMaybe False optEnableProfiling || optEnableExecutableProfiling)
              & metaSection.maintainers .~ Set.fromList (map (review ident) optMaintainer)
--            & metaSection.platforms .~ Set.fromList optPlatform
              & doCheck &&~ optDoCheck
              & extraFunctionArgs %~ Set.union (Set.fromList ("inherit stdenv":map (fromString . ("inherit " ++)) optExtraArgs))

      shell :: Doc
      shell = vcat
              [ text "{ nixpkgs ? import <nixpkgs> {}, compiler ? \"default\" }:"
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
              , text "  drv = haskellPackages.callPackage f {};"
              , text ""
              , text "in"
              , text ""
              , text "  if pkgs.lib.inNixShell then drv.env else drv"
              ]
  pure $ if optNixShellOutput then Left shell else Right deriv

cabal2nix :: [String] -> IO ()
cabal2nix args = do
  v <- cabal2nix' args
  print $ case v of
    Left shell -> shell
    Right d -> pPrint d

readFlagList :: [String] -> FlagAssignment
readFlagList = map tagWithValue
  where tagWithValue ('-':fname) = (mkFlagName (lowercase fname), False)
        tagWithValue fname       = (mkFlagName (lowercase fname), True)

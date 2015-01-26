{-# LANGUAGE RecordWildCards #-}

module Main ( main ) where

import Cabal2Nix.Generate ( cabal2nix )
import Cabal2Nix.Normalize ( normalize )
import Cabal2Nix.Package
import Cabal2Nix.Version
import Control.Exception ( bracket )
import Control.Lens
import Data.Maybe ( fromMaybe )
import qualified Data.Set as Set
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Meta
import Distribution.Nixpkgs.Util.PrettyPrinting hiding ( (<>) )
import Distribution.PackageDescription ( FlagName(..), FlagAssignment )
import Distribution.Simple.Utils ( lowercase )
import Options.Applicative
import System.IO ( hFlush, stdout, stderr )
import qualified Text.PrettyPrint.ANSI.Leijen as P2 hiding ( (<$>), (<>) )

data Options = Options
  { optSha256 :: Maybe String
  , optMaintainer :: [String]
  , optPlatform :: [String]
  , optHaddock :: Bool
  , optDoCheck :: Bool
  , optJailbreak :: Bool
  , optRevision :: Maybe String
  , optHyperlinkSource :: Bool
  , optHackageDb :: Maybe FilePath
  , optNixShellOutput :: Bool
  , optFlags :: [String]
  , optUrl :: String
  }
  deriving (Show)

options :: Parser Options
options = Options
          <$> optional (strOption $ long "sha256" <> metavar "HASH" <> help "sha256 hash of source tarball")
          <*> many (strOption $ long "maintainer" <> metavar "MAINTAINER" <> help "maintainer of this package (may be specified multiple times)")
          <*> many (strOption $ long "platform" <> metavar "PLATFORM" <> help "supported build platforms (may be specified multiple times)")
          <*> flag True False (long "no-haddock" <> help "don't run Haddock when building this package")
          <*> flag True False (long "no-check" <> help "don't run regression test suites of this package")
          <*> switch (long "jailbreak" <> help "disregard version restrictions on build inputs")
          <*> optional (strOption $ long "revision" <> help "revision to use when fetching from VCS")
          <*> flag True False (long "no-hyperlink-source" <> help "don't generate pretty-printed source code for the documentation")
          <*> optional (strOption $ long "hackage-db" <> metavar "PATH" <> help "path to the local hackage db in tar format")
          <*> switch (long "shell" <> help "generate output suitable for nix-shell")
          <*> many (strOption $ short 'f' <> long "flag" <> help "Cabal flag (may be specified multiple times)")
          <*> strArgument (metavar "URI")

pinfo :: ParserInfo Options
pinfo = info
        (   helper
        <*> infoOption ("cabal2nix " ++ version) (long "version" <> help "Show version number")
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
                                                      ++ "souce for the derivation and the information is taken from the cabal file "
                                                      ++ "at the root of the downloaded source."
                                                      )))
                     ]))
        )

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() -> do
  Options {..} <- execParser pinfo

  pkg <- getPackage optHackageDb $ Source optUrl (fromMaybe "" optRevision) (maybe UnknownHash Guess optSha256)

  let flags = readFlagList optFlags

      deriv :: Derivation
      deriv = cabal2nix flags (pkgCabal pkg)
              & src .~ pkgSource pkg
              & runHaddock .~ optHaddock
              & jailbreak .~ optJailbreak
              & hyperlinkSource .~ optHyperlinkSource
              & metaSection.maintainers .~ Set.fromList optMaintainer
              & metaSection.platforms .~ Set.fromList optPlatform
              & doCheck &&~ optDoCheck
              & extraFunctionArgs . contains "stdenv" .~ True

      deriv' :: Doc
      deriv' = pPrint (normalize deriv)

      shell :: Doc
      shell = vcat
              [ text "{ nixpkgs ? import <nixpkgs> {}, compiler ? \"ghc7101\" }:"
              , text ""
              , text "let"
              , text ""
              , text "  inherit (nixpkgs) pkgs;"
              , text ""
              , hcat [ text "  f = ", deriv', semi ]
              , text ""
              , text "  drv = pkgs.haskell.packages.${compiler}.callPackage f {};"
              , text ""
              , text "in"
              , text ""
              , text "  if pkgs.lib.inNixShell then drv.env else drv"
              ]

  print (if optNixShellOutput then shell else deriv')

readFlagList :: [String] -> FlagAssignment
readFlagList = map tagWithValue
  where tagWithValue ('-':fname) = (FlagName (lowercase fname), False)
        tagWithValue fname       = (FlagName (lowercase fname), True)

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main ( main ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Index as TarIndex
import Control.Exception
import Control.Lens hiding ( argument, (<.>) )
import Control.Monad
import Data.Aeson hiding ( Options )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLUTF8
import Data.Function ( on )
import Data.List ( intercalate, isPrefixOf, maximumBy )
import Data.List.Split
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.Ord
import qualified Data.Set as Set
import Data.String
import Data.Typeable ( Typeable )
import Distribution.Compiler
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Hashes
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Haskell.FromCabal.Flags
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import Distribution.Package ( packageId )
import Distribution.PackageDescription ( mkFlagName, mkFlagAssignment, FlagAssignment )
import Distribution.PackageDescription.Parsec
import Distribution.Parsec as P
import Distribution.Pretty ( pretty )
import Distribution.Simple.Utils ( lowercase )
import Distribution.System
import Distribution.Types.Dependency
import Distribution.Verbosity ( silent )
import Distribution.Version
import GHC.Generics ( Generic )
import Language.Nix
import Options.Applicative
import qualified Paths_cabal2nix_v3 as Cabal2nix ( version )
import System.Environment ( getArgs )
import System.FilePath
import System.IO
import Text.PrettyPrint.HughesPJClass ( Pretty(..), prettyShow )

{-# ANN module ("HLint: ignore Use Just" :: String) #-}

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() ->
         getArgs >>= cabal2nix

cabal2nix :: [String] -> IO ()
cabal2nix = parseArgs >=> cabal2nix' >=> putStrLn . prettyShow

cabal2nix' :: Options -> IO Derivation
cabal2nix' opts@Options {..} = do
  let tarFile = "/home/simons/.cabal/packages/hackage.haskell.org/01-index.tar"
      idxFile = tarFile <.> "idx"
  buf <- BS.readFile idxFile
  idx <- case TarIndex.deserialise buf of
           Nothing      -> fail ("cannot read tar index file at " <> show idxFile)
           Just (idx,_) -> return idx
  let pkgName = prettyShow (depPkgName optPkgSpec)
      vrange = depVerRange optPkgSpec
  versions' <- case TarIndex.lookup idx pkgName of
                 Just (TarIndex.TarDir vs) -> return vs
                 _                         -> fail (show pkgName <> " is not a valid Hackage package")
  let versions = [ (v,e) | (x,e) <- versions', Just v <- [simpleParsec x], v `withinRange` vrange ]
  when (null versions) $
    fail ("no versions on Hackage satisfy the constraint " <> show (prettyShow optPkgSpec))
  let (pkgVersion, files) = case maximumBy (comparing fst) versions of
                              (v, TarIndex.TarDir files) -> (v, files)
                              huh                        -> error ("unexpected contents in Hackage tarball: " <> show huh)
  (cabalIdx, metaIdx) <- case (lookup (pkgName <.> "cabal") files, lookup "package.json" files) of
    (Just (TarIndex.TarFileEntry idx1), Just (TarIndex.TarFileEntry idx2)) -> return (idx1, idx2)
    _                      -> fail ("unexpected contents in Hackage tarball: " <> show files)
  (cabalBuf, metaBuf) <- withFile tarFile ReadMode $ \fh -> do
     cabalEntry <- TarIndex.hReadEntry fh cabalIdx
     metaEntry <- TarIndex.hReadEntry fh metaIdx
     case (Tar.entryContent cabalEntry, Tar.entryContent metaEntry) of
       (Tar.NormalFile cabalBuf _, Tar.NormalFile metaBuf _) -> return (cabalBuf, metaBuf)
       _                                                     -> fail ("unexpected contents in Hackage tarball: " <> show (cabalEntry, metaEntry))

  gpd <- case parseGenericPackageDescriptionMaybe (BSL.toStrict cabalBuf) of
           Just gpd -> return gpd
           Nothing  -> fail "failed to parse Cabal file"

  let meta = parseMetaData metaBuf
      targetPath = "<repo>/package/" <> pkgName <> "-" <> prettyShow pkgVersion <> ".tar.gz"

      targetData = case Map.lookup targetPath (targets (signed meta)) of
                     Just tgt -> tgt
                     Nothing  -> error ("unexpected data in packages.json file: " <> show metaBuf)

      sha256Hash = case Map.lookup "sha256" (hashes targetData) of
                     Just hash -> printSHA256 (packHex hash)
                     Nothing   -> error ("cannot find sha256 hash in packages.json file: " <> show metaBuf)

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
              & src .~ DerivationSource "" "mirror://hackage/foobar" "" sha256Hash Nothing
              & extraFunctionArgs %~ Set.union (Set.fromList ("inherit stdenv":map (fromString . ("inherit " ++)) optExtraArgs))
              & extraAttributes %~ Map.union (readExtraAttributes optExtraAttributes)
  return deriv

parseArgs :: [String] -> IO Options
parseArgs = handleParseResult . execParserPure defaultPrefs pinfo

pinfo :: ParserInfo Options
pinfo = info
        (   helper
        <*> infoOption ("cabal2nix " ++ prettyShow Cabal2nix.version) (long "version" <> help "Show version number")
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
  , optPkgSpec :: Dependency
  , optExtraAttributes :: [String]
  }
  deriving Show

options :: Parser Options
options = do
  optFlags <- many (strOption $ short 'f' <> long "flag" <> help "Cabal flag (may be specified multiple times)")
  optCompiler <- option parseCabal (long "compiler" <> help "compiler to use when evaluating the Cabal file" <> value buildCompilerId <> showDefaultWith prettyShow)
  optSystem <- option (maybeReader parsePlatform) (long "system" <> help "host system (in either short Nix format or full LLVM style) to use when evaluating the Cabal file" <> value buildPlatform <> showDefaultWith prettyShow)
  optExtraArgs <- many (strOption $ long "extra-arguments" <> help "extra parameters required for the function body")
  optPkgSpec <- argument parseCabal (metavar "VERSION-SPEC")
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


instance Pretty Dependency where
  pPrint = pretty

----- Parse the meta data file

-- | Parse the @package.json@ file found in a @01-index.tar@ tarball from
-- Hackage with "Data.Aeson". This function is a convenience wrapper around
-- 'eitherDecode' that throws an 'InvalidMetaFile' exception to signal failure.
--
-- >>> :set -XOverloadedStrings
-- >>> parseMetaData "{\"signatures\":[],\"signed\":{\"_type\":\"Targets\",\"expires\":null,\"targets\":{\"<repo>/package/jailbreak-cabal-1.3.2.tar.gz\":{\"hashes\":{\"md5\":\"ba42b3e68323ccbeb3ac900cd68f9e90\",\"sha256\":\"212a8bbc3dfc748c4063282414a2726709d651322f3984c9989179d2352950f4\"},\"length\":2269}},\"version\":0}}"
-- MetaData {signed = SignedMetaData {version = 0, expires = Nothing, _type = "Targets", targets = fromList [("<repo>/package/jailbreak-cabal-1.3.2.tar.gz",TargetData {length = 2269, hashes = fromList [("md5","ba42b3e68323ccbeb3ac900cd68f9e90"),("sha256","212a8bbc3dfc748c4063282414a2726709d651322f3984c9989179d2352950f4")]})]}, signatures = []}

parseMetaData :: BSLUTF8.ByteString -> MetaData
parseMetaData = either (throw . InvalidMetaFile) id . eitherDecode

data MetaData = MetaData { signed :: SignedMetaData
                         , signatures :: [String]
                         }
  deriving (Show, Generic)

instance FromJSON MetaData

data SignedMetaData = SignedMetaData { version :: Int
                                     , expires :: Maybe String
                                     , _type   :: String
                                     , targets :: Map String TargetData
                                     }
  deriving (Show, Generic)

instance FromJSON SignedMetaData

data TargetData = TargetData { length :: Int
                             , hashes :: Map String String
                             }
  deriving (Show, Generic)

instance FromJSON TargetData

newtype InvalidMetaFile = InvalidMetaFile String deriving (Show, Typeable)
instance Exception InvalidMetaFile

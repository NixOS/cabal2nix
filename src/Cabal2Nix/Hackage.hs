module Cabal2Nix.Hackage ( readHashedHackage, module Distribution.Hackage.DB ) where

import Data.ByteString.Lazy ( ByteString )
import Data.Digest.Pure.SHA ( sha256, showDigest )
import Distribution.Hackage.DB
import qualified Distribution.Hackage.DB.Parsed as Unparsed ( parsePackage )
import qualified Distribution.Hackage.DB.Unparsed as Unparsed

-- | A variant of 'readHackage' that adds the SHA256 digest of the
-- original Cabal file to the parsed 'GenericPackageDescription'. That
-- hash is required to build packages with an "edited" cabal file,
-- because Nix needs to download the edited file and patch it into the
-- original tarball.

readHashedHackage :: IO Hackage
readHashedHackage = fmap parseUnparsedHackage Unparsed.readHackage
  where
    parseUnparsedHackage :: Unparsed.Hackage -> Hackage
    parseUnparsedHackage = mapWithKey (mapWithKey . parsePackage)

    parsePackage :: String -> Version -> ByteString -> GenericPackageDescription
    parsePackage name version buf =
      let pkg = Unparsed.parsePackage name version buf
          hash = showDigest (sha256 buf)
      in
       pkg { packageDescription = (packageDescription pkg) {
                customFieldsPD = ("x-cabal-file-hash", hash) : (customFieldsPD (packageDescription pkg))
                }
           }

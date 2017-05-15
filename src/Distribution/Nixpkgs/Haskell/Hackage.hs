module Distribution.Nixpkgs.Haskell.Hackage ( readHashedHackage, readHashedHackage', module Distribution.Hackage.DB ) where

import Data.ByteString.Lazy ( ByteString )
import Data.ByteString.Char8 ( unpack )
import Distribution.Hackage.DB
import qualified Distribution.Hackage.DB.Parsed as Unparsed ( parsePackage )
import qualified Distribution.Hackage.DB.Unparsed as Unparsed
import OpenSSL.Digest ( digest, digestByName, toHex )

-- | A variant of 'readHackage' that adds the SHA256 digest of the
-- original Cabal file to the parsed 'GenericPackageDescription'. That
-- hash is required to build packages with an "edited" cabal file,
-- because Nix needs to download the edited file and patch it into the
-- original tarball.

readHashedHackage :: IO Hackage
readHashedHackage = hackagePath >>= readHashedHackage'

readHashedHackage' :: FilePath -> IO Hackage
readHashedHackage' = fmap parseUnparsedHackage . Unparsed.readHackage'
  where
    parseUnparsedHackage :: Unparsed.Hackage -> Hackage
    parseUnparsedHackage = mapWithKey (mapWithKey . parsePackage)

    parsePackage :: String -> Version -> ByteString -> GenericPackageDescription
    parsePackage name version buf =
      let pkg = Unparsed.parsePackage name version buf
          hash = unpack (toHex (digest (digestByName "sha256") buf))
      in
       pkg { packageDescription = (packageDescription pkg) {
                customFieldsPD = ("X-Cabal-File-Hash", hash) : customFieldsPD (packageDescription pkg)
                }
           }

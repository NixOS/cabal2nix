module Main ( main ) where

import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Monad (forM_)
import Data.Aeson (decodeFileStrict)
import Data.Bifunctor (second)
import Data.Maybe (fromJust)
import Distribution.Nixpkgs.License
import Distribution.Nixpkgs.Meta
import Distribution.System (Platform (..), OS (..), Arch (..))
import Language.Nix.Identifier
import Language.Nix.PrettyPrinting
import System.Directory (doesFileExist)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "DeepSeq instances work properly for" $ do
    it "License" $ mapM_ hitsBottom [Known undefined, Unknown (Just undefined)]
    it "Meta" $ mapM_ hitsBottom
                  [ nullMeta & homepage .~ undefined
                  , nullMeta & description .~ undefined
                  , nullMeta & license .~ undefined
                  , nullMeta & platforms .~ undefined
                  , nullMeta & maintainers .~ undefined
                  , nullMeta & broken .~ undefined
                  ]

  describe "Platform rendering and parsing:" $ do
    it "Checks cover all systems from all-system-tuples.json" $do
      let allSystemTuplesJson = "test/data/all-system-tuples.json"
      available <- doesFileExist allSystemTuplesJson
      if not available
      then pendingWith $ "System tuple list not found at: " ++ allSystemTuplesJson
      else do
        allSystemTuples <- decodeFileStrict allSystemTuplesJson
        map fst nixpkgsSystemMapping `shouldMatchList` fromJust allSystemTuples

    forM_ platformMapping $ \(str, plat) -> describe str $ do
      it "is rendered correctly" $ checkRendering plat str
      it "is parsed correctly" $
        nixpkgsPlatformFromString str `shouldBe` Just plat

-- All system tuples from lib.platforms as of 2022-05-08. Testing these allows
-- us to get notified about behavior change in Cabal as early as possible.
nixpkgsSystemMapping :: [(String, Platform)]
nixpkgsSystemMapping =
  [ -- lib.platforms.all
    ("aarch64-darwin", Platform AArch64 OSX)
  , ("aarch64-genode", Platform AArch64 (OtherOS "genode"))
  , ("aarch64-linux", Platform AArch64 Linux)
  , ("aarch64-netbsd", Platform AArch64 NetBSD)
  , ("aarch64-none", Platform AArch64 (OtherOS "none"))
  , ("aarch64_be-none", Platform (OtherArch "aarch64_be") (OtherOS "none"))
  , ("arm-none", Platform Arm (OtherOS "none"))
  , ("armv5tel-linux", Platform (OtherArch "armv5tel") Linux)
  , ("armv6l-linux", Platform (OtherArch "armv6l") Linux)
  , ("armv6l-netbsd", Platform (OtherArch "armv6l") NetBSD)
  , ("armv6l-none", Platform (OtherArch "armv6l") (OtherOS "none"))
  , ("armv7a-darwin", Platform (OtherArch "armv7a") OSX)
  , ("armv7a-linux", Platform (OtherArch "armv7a") Linux)
  , ("armv7a-netbsd", Platform (OtherArch "armv7a") NetBSD)
  , ("armv7l-linux", Platform (OtherArch "armv7l") Linux)
  , ("armv7l-netbsd", Platform (OtherArch "armv7l") NetBSD)
  , ("avr-none", Platform (OtherArch "avr") (OtherOS "none"))
  , ("i686-cygwin", Platform I386 (OtherOS "cygwin"))
  , ("i686-darwin", Platform I386 OSX)
  , ("i686-freebsd", Platform I386 FreeBSD)
  , ("i686-genode", Platform I386 (OtherOS "genode"))
  , ("i686-linux", Platform I386 Linux)
  , ("i686-netbsd", Platform I386 NetBSD)
  , ("i686-none", Platform I386 (OtherOS "none"))
  , ("i686-openbsd", Platform I386 OpenBSD)
  , ("i686-windows", Platform I386 Windows)
  , ("js-ghcjs", Platform JavaScript Ghcjs)
  , ("m68k-linux", Platform M68k Linux)
  , ("m68k-netbsd", Platform M68k NetBSD)
  , ("m68k-none", Platform M68k (OtherOS "none"))
  , ("mips64el-linux", Platform (OtherArch "mips64el") Linux)
  , ("mipsel-linux", Platform (OtherArch "mipsel") Linux)
  , ("mipsel-netbsd", Platform (OtherArch "mipsel") NetBSD)
  , ("mmix-mmixware", Platform (OtherArch "mmix") (OtherOS "mmixware"))
  , ("msp430-none", Platform (OtherArch "msp430") (OtherOS "none"))
  , ("or1k-none", Platform (OtherArch "or1k") (OtherOS "none"))
  , ("powerpc-netbsd", Platform PPC NetBSD)
  , ("powerpc-none", Platform PPC (OtherOS "none"))
  , ("powerpc64-linux", Platform PPC64 Linux)
  , ("powerpc64le-linux", Platform (OtherArch "powerpc64le") Linux)
  , ("powerpcle-none", Platform (OtherArch "powerpcle") (OtherOS "none"))
  , ("riscv32-linux", Platform (OtherArch "riscv32") Linux)
  , ("riscv32-netbsd", Platform (OtherArch "riscv32") NetBSD)
  , ("riscv32-none", Platform (OtherArch "riscv32") (OtherOS "none"))
  , ("riscv64-linux", Platform (OtherArch "riscv64") Linux)
  , ("riscv64-netbsd", Platform (OtherArch "riscv64") NetBSD)
  , ("riscv64-none", Platform (OtherArch "riscv64") (OtherOS "none"))
  , ("s390-linux", Platform S390 Linux)
  , ("s390-none", Platform S390 (OtherOS "none"))
  , ("s390x-linux", Platform (OtherArch "s390x") Linux)
  , ("s390x-none", Platform (OtherArch "s390x") (OtherOS "none"))
  , ("vc4-none", Platform (OtherArch "vc4") (OtherOS "none"))
  , ("wasm32-wasi", Platform (OtherArch "wasm32") (OtherOS "wasi"))
  , ("wasm64-wasi", Platform (OtherArch "wasm64") (OtherOS "wasi"))
  , ("x86_64-cygwin", Platform X86_64 (OtherOS "cygwin"))
  , ("x86_64-darwin", Platform X86_64 OSX)
  , ("x86_64-freebsd", Platform X86_64 FreeBSD)
  , ("x86_64-genode", Platform X86_64 (OtherOS "genode"))
  , ("x86_64-linux", Platform X86_64 Linux)
  , ("x86_64-netbsd", Platform X86_64 NetBSD)
  , ("x86_64-none", Platform X86_64 (OtherOS "none"))
  , ("x86_64-openbsd", Platform X86_64 OpenBSD)
  , ("x86_64-redox", Platform X86_64 (OtherOS "redox"))
  , ("x86_64-solaris", Platform X86_64 Solaris)
  , ("x86_64-windows", Platform X86_64 Windows)
  -- lib.systems.examples
  , ("mips-linux", Platform Mips Linux)
  , ("mips64-linux", Platform (OtherArch "mips64") Linux)
  ]

platformMapping :: [(String, NixpkgsPlatform)]
platformMapping = map (second NixpkgsPlatformSingle) nixpkgsSystemMapping
  ++ map platformGroupMapping [ "riscv", "x86", "linux", "darwin" ]

platformGroupMapping :: String -> (String, NixpkgsPlatform)
platformGroupMapping name =
  ("lib.platforms." ++ name, NixpkgsPlatformGroup (ident # name))

checkRendering :: NixpkgsPlatform -> String -> Expectation
checkRendering plat@(NixpkgsPlatformSingle _) str =
  prettyShow plat `shouldBe` "\"" ++ str ++ "\""
checkRendering plat str = prettyShow plat `shouldBe` str

hitsBottom :: NFData a => a -> Expectation
hitsBottom x = evaluate (rnf x) `shouldThrow` anyErrorCall

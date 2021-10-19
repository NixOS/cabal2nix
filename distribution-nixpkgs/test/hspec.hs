module Main ( main ) where

import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Monad (forM_)
import Distribution.Nixpkgs.License
import Distribution.Nixpkgs.Meta
import Distribution.System (Platform (..), OS (..), Arch (..))
import Language.Nix.Identifier
import Language.Nix.PrettyPrinting
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

  describe "Platform is rendered correctly for" $ forM_ platformMapping $
    \(plat, str) -> it str $ checkRendering plat str

-- All system tuples from lib.platforms as of 2021-10-20. Testing these allows
-- us to get notified about behavior change in Cabal as early as possible.
platformMapping :: [(NixpkgsPlatform, String)]
platformMapping = (map (bimap NixpkgsPlatformSingle id)
  [ (Platform (OtherArch "armv5tel") Linux, "armv5tel-linux")
  , (Platform (OtherArch "armv6l") (OtherOS "none"), "armv6l-none")
  , (Platform (OtherArch "armv6l") Linux, "armv6l-linux")
  , (Platform (OtherArch "armv6l") NetBSD, "armv6l-netbsd")
  , (Platform (OtherArch "armv7a") Linux, "armv7a-linux")
  , (Platform (OtherArch "armv7a") NetBSD, "armv7a-netbsd")
  , (Platform (OtherArch "armv7a") OSX, "armv7a-darwin")
  , (Platform (OtherArch "armv7l") Linux, "armv7l-linux")
  , (Platform (OtherArch "armv7l") NetBSD, "armv7l-netbsd")
  , (Platform (OtherArch "avr") (OtherOS "none"), "avr-none")
  , (Platform (OtherArch "mipsel") Linux, "mipsel-linux")
  , (Platform (OtherArch "mipsel") NetBSD, "mipsel-netbsd")
  , (Platform (OtherArch "mmix") (OtherOS "mmixware"), "mmix-mmixware")
  , (Platform (OtherArch "msp430") (OtherOS "none"), "msp430-none")
  , (Platform (OtherArch "or1k") (OtherOS "none"), "or1k-none")
  , (Platform (OtherArch "powerpc64le") Linux, "powerpc64le-linux")
  , (Platform (OtherArch "riscv32") (OtherOS "none"), "riscv32-none")
  , (Platform (OtherArch "riscv32") Linux, "riscv32-linux")
  , (Platform (OtherArch "riscv32") NetBSD, "riscv32-netbsd")
  , (Platform (OtherArch "riscv64") (OtherOS "none"), "riscv64-none")
  , (Platform (OtherArch "riscv64") Linux, "riscv64-linux")
  , (Platform (OtherArch "riscv64") NetBSD, "riscv64-netbsd")
  , (Platform (OtherArch "s390x") (OtherOS "none"), "s390x-none")
  , (Platform (OtherArch "s390x") Linux, "s390x-linux")
  , (Platform (OtherArch "vc4") (OtherOS "none"), "vc4-none")
  , (Platform (OtherArch "wasm32") (OtherOS "wasi"), "wasm32-wasi")
  , (Platform (OtherArch "wasm64") (OtherOS "wasi"), "wasm64-wasi")
  , (Platform AArch64 (OtherOS "genode"), "aarch64-genode")
  , (Platform AArch64 (OtherOS "none"), "aarch64-none")
  , (Platform AArch64 Linux, "aarch64-linux")
  , (Platform AArch64 NetBSD, "aarch64-netbsd")
  , (Platform AArch64 OSX, "aarch64-darwin")
  , (Platform Arm (OtherOS "none"), "arm-none")
  , (Platform I386 (OtherOS "cygwin"), "i686-cygwin")
  , (Platform I386 (OtherOS "genode"), "i686-genode")
  , (Platform I386 (OtherOS "none"), "i686-none")
  , (Platform I386 FreeBSD , "i686-freebsd")
  , (Platform I386 Linux, "i686-linux")
  , (Platform I386 NetBSD, "i686-netbsd")
  , (Platform I386 OSX, "i686-darwin")
  , (Platform I386 OpenBSD, "i686-openbsd")
  , (Platform I386 Windows, "i686-windows")
  , (Platform JavaScript Ghcjs, "js-ghcjs")
  , (Platform M68k (OtherOS "none"), "m68k-none")
  , (Platform M68k Linux, "m68k-linux")
  , (Platform M68k NetBSD, "m68k-netbsd")
  , (Platform PPC (OtherOS "none"), "powerpc-none")
  , (Platform PPC NetBSD, "powerpc-netbsd")
  , (Platform PPC64 Linux, "powerpc64-linux")
  , (Platform S390 (OtherOS "none"), "s390-none")
  , (Platform S390 Linux, "s390-linux")
  , (Platform X86_64 (OtherOS "cygwin"), "x86_64-cygwin")
  , (Platform X86_64 (OtherOS "genode"), "x86_64-genode")
  , (Platform X86_64 (OtherOS "none"), "x86_64-none")
  , (Platform X86_64 (OtherOS "redox"), "x86_64-redox")
  , (Platform X86_64 FreeBSD, "x86_64-freebsd")
  , (Platform X86_64 Linux, "x86_64-linux")
  , (Platform X86_64 NetBSD, "x86_64-netbsd")
  , (Platform X86_64 OSX, "x86_64-darwin")
  , (Platform X86_64 OpenBSD, "x86_64-openbsd")
  , (Platform X86_64 Solaris, "x86_64-solaris")
  , (Platform X86_64 Windows, "x86_64-windows")
  ]) ++ (map platformGroupMapping [ "riscv", "x86", "linux", "darwin" ])

platformGroupMapping :: String -> (NixpkgsPlatform, String)
platformGroupMapping name =
  (NixpkgsPlatformGroup (ident # name), "lib.platforms." ++ name)

checkRendering :: NixpkgsPlatform -> String -> Bool
checkRendering plat@(NixpkgsPlatformSingle _) str =
  prettyShow plat == "\"" ++ str ++ "\""
checkRendering plat str = prettyShow plat == str

hitsBottom :: NFData a => a -> Expectation
hitsBottom x = evaluate (rnf x) `shouldThrow` anyErrorCall

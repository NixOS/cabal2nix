{-# LANGUAGE LambdaCase #-}
{-|
Description: Parse platform strings used by nixpkgs into Cabal's 'Platform'

This module defines conversions from the (autoconf-derived) platform strings
nixpkgs uses into Cabal's 'Platform' type. This is intended to facilitate later
evaluation of @.cabal@ files. For this conversion Cabal's 'Permissive'
heuristics are used as well as a logic equivalent to the @GHC_CONVERT_*@ macros
from GHC's configure script.

Since the process is inherently lossy because Cabal ignores certain factors like
endianness, conversion from 'Platform' to nixpkgs' platform strings. For this
usecase, try "Distribution.Nixpkgs.Meta" from @distribution-nixpkgs@.
-}
module Distribution.Nixpkgs.Haskell.Platform
  ( parsePlatformLenient
  , parsePlatformFromSystemLenient
  ) where

import Data.List ( isPrefixOf, intercalate )
import Data.List.Split ( splitOn )
import Data.Maybe ( fromMaybe, listToMaybe )
import Distribution.System

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

parsePlatformParts :: [String] -> Maybe Platform
parsePlatformParts = \case
  [arch, os] ->
    Just $ Platform (parseArch arch) (parseOS os)
  (arch : _ : osParts) ->
    Just $ Platform (parseArch arch) $ parseOS $ intercalate "-" osParts
  _ -> Nothing

-- | Convert a platform string of two or three(-ish) components to 'Platform'.
--
--   For this, the following logic is utilized:
--
--   - If the string has one dash, the form @cpu-os@ is assumed where @os@ may
--     only have a single component. The @vendor@ part is ignored.
--
--   - Otherwise @cpu-vendor-os@ is assumed where @os@ may have any number of
--     components separated by dashes to accomodate its two component
--     @kernel-system@ form.
--
--   __Note:__ This behavior is different from nixpkgs' @lib.systems.elaborate@:
--   Because we have no knowledge of the legal contents of the different parts,
--   we only decide how to parse it based on what form the string has. This can
--   give different results compared to autoconf or nixpkgs. It will also never
--   reject an invalid platform string that has a valid form.
--
--   >>> parsePlatformLenient "x86_64-unknown-linux"
--   Just (Platform X86_64 Linux)
--   >>> parsePlatformLenient "x86_64-pc-linux-gnu"
--   Just (Platform X86_64 Linux)
--   >>> parsePlatformLenient "x86_64-linux"
--   Just (Platform X86_64 Linux)
--
--   __Note__ also that this conversion sometimes looses information nixpkgs
--   would retain:
--
--   >>> parsePlatformLenient "powerpc64-unknown-linux"
--   Just (Platform PPC64 Linux)
--   >>> parsePlatformLenient "powerpc64le-unknown-linux"
--   Just (Platform PPC64 Linux)
parsePlatformLenient :: String -> Maybe Platform
parsePlatformLenient = parsePlatformParts . splitOn "-"

-- | Convert a Nix style system tuple into a Cabal 'Platform'. The tuple is
--   assumed to be of the form @cpu-os@, any extra components are assumed to be
--   part of @os@ to accomodate its @kernel-system@ form.
--
--   The same caveats about validation and lossiness apply as for
--   'parsePlatformLenient'.
--
--   >>> parsePlatformFromSystemLenient "x86_64-linux"
--   Just (Platform X86_64 Linux)
--   >>> parsePlatformFromSystemLenient "x86_64-linux-musl"
--   Just (Platform X86_64 Linux)
--   >>> parsePlatformFromSystemLenient "i686-netbsd"
--   Just (Platform I386 NetBSD)
parsePlatformFromSystemLenient :: String -> Maybe Platform
parsePlatformFromSystemLenient s =
  case break (== '-') s of
    (arch, '-':os) ->
      if null arch || null os
      then Nothing
      else parsePlatformParts [arch, os]
    _ -> Nothing

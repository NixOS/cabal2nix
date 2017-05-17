-- | Render SHA message digests in the peculiar base32'ish format used by Nix.

module Distribution.Nixpkgs.Hashes ( printSHA256, packHex ) where

import Control.Exception ( assert )
import Data.Bits
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.Word

base32chars :: ByteString   -- omitted: E O U T
base32chars = BSC.pack "0123456789abcdfghijklmnpqrsvwxyz";

-- | Render a SHA265 message digest into the unusual base32 scheme used by Nix.
--
-- >>> printSHA256 (packHex "7459ca5c6e117538122f04caf3dbfc58303028c26c58943430c16ff28a3b1d49")
-- "0j8x7f5g4vy160s98n3cq8l30c2qzkdz7jh45w93hx8idrfclnbl"

printSHA256 :: ByteString -> String
printSHA256 buf = assert (BS.length buf == 32) $
  map (BSC.index base32chars . fromIntegral . getQuintet buf) [51,50..0]

getQuintet :: ByteString -> Int -> Word8
getQuintet buf n = (c1 .|. c2) .&. 0x1f
  where
    b     = n * 5
    (i,j) = b `divMod` 8
    c1    = BS.index buf i `shiftR` j
    c2    = if i >= 31 then 0 else BS.index buf (i+1) `shiftL` (8-j)

-- | Parse a hexadecimal hash representation into its binary form suitable for
-- encoding with 'printSHA256'.
--
-- >>> packHex "48656c6c6f2c20776f726c642e"
-- "Hello, world."
--
-- Leading zeros can be omitted:
--
-- >>> packHex "0"
-- "\NUL"

packHex :: String -> ByteString
packHex = BS.pack . hex2bin

hex2bin :: String -> [Word8]
hex2bin input = f $ (if even (length input) then id else ('0':)) input
  where
    f :: String -> [Word8]
    f []         = []
    f [x]        = [digit x]
    f (x1:x2:xs) = ((digit x1 `shiftL` 4) .|. digit x2) : f xs

    digit :: Char -> Word8
    digit = fromIntegral . digitToInt

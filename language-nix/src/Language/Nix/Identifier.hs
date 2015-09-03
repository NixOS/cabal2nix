{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Language.Nix.Identifier
  (
    -- | Identifiers in Nix are essentially strings. They can be constructed
    -- (and viewed) with the 'ident' isomorphism. For the sake of convenience,
    -- @Identifier@s are an instance of the 'IsString' class.
    --
    -- Reasonable people restrict themselves to identifiers of the form
    -- @[a-zA-Z\_][a-zA-Z0-9\_\'\-]*@, because these don't need quoting. The
    -- methods of the 'Pretty' class can be used to pretty-print an identifier
    -- with proper quoting:
    --
    -- >>> pPrint (ident # "test")
    -- test
    -- >>> pPrint (ident # "foo.bar")
    -- "foo.bar"
    Identifier

  , -- | An isomorphism that allows conversion of 'Identifier' from/to the
    -- standard 'String' type via 'review'.
    --
    -- prop> \str -> fromString str == ident # str
    -- prop> \str -> set ident str undefined == ident # str
    -- prop> \str -> view ident (review ident str) == str
    ident

  , -- | Helper function to quote a given identifier string if necessary.
    --
    -- >>> putStrLn (quote "abc")
    -- abc
    -- >>> putStrLn (quote "abc.def")
    -- "abc.def"
    quote

  , -- | Checks whether a given string needs quoting when interpreted as an
    -- 'Identifier'.
    needsQuoting

  ) where

import Control.DeepSeq
import Control.Lens
import Data.String
import Text.PrettyPrint.HughesPJClass
import Text.Regex.Posix

declareLenses [d| newtype Identifier = Identifier { ident :: String }
                    deriving (Show, Eq, Ord, IsString)
              |]

instance Pretty Identifier where
  pPrint i = text (i ^. ident . to quote)

instance NFData Identifier where rnf (Identifier str) = rnf str

needsQuoting :: String -> Bool
needsQuoting str = not (str =~ grammar)
  where grammar :: String       -- TODO: should be a compiled regular expression
        grammar = "^[a-zA-Z\\_][a-zA-Z0-9\\_\\'\\-]*$"

quote :: String -> String
quote s = if needsQuoting s then show s else s

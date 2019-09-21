{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Nix.Binding ( Binding, binding, localName, reference ) where

import Control.DeepSeq
import Control.Lens
import Data.Maybe
import Data.String
import GHC.Generics ( Generic )
import Language.Nix.Identifier
import Language.Nix.Path
import Test.QuickCheck
import Text.Parsec.Class as P
import Text.PrettyPrint.HughesPJClass as PP

-- | A 'Binding' represents an identifier that refers to some other 'Path'.
--
-- >>> :set -XOverloadedStrings
-- >>> "inherit (foo.bar) abc" :: Binding
-- Bind (Identifier "abc") (Path [Identifier "foo",Identifier "bar",Identifier "abc"])
--
-- prop> \b -> Just (b :: Binding) == parseM "Binding" (prettyShow b)

declareLenses [d| data Binding = Bind { localName :: Identifier, reference :: Path }
                    deriving (Show, Eq, Ord, Generic)
              |]

binding :: Iso' Binding (Identifier,Path)
binding = iso (\(Bind l r) -> (l,r)) (uncurry Bind)

instance NFData Binding where
  rnf (Bind l r) = l `deepseq` rnf r

instance Arbitrary Binding where
  arbitrary = review binding <$> arbitrary

instance CoArbitrary Binding

instance Pretty Binding where
  pPrint b = case (init ps, last ps) of
               ([], i') -> if i == i'
                              then text "inherit" <+> pPrint i'
                              else pPrint i <+> equals <+> pPrint p
               (p', i') -> if i == i'
                              then text "inherit" <+> parens (pPrint (path # p')) <+> pPrint i'
                              else pPrint i <+> equals <+> pPrint p

          where
            (i, p) = view binding b
            ps = view path p

instance HasParser Binding where
  parser = try parseInherit <|> parseAssignment

instance IsString Binding where
  fromString = parse "Language.Nix.Binding.Binding"

parseAssignment :: CharParser st tok m Binding
parseAssignment = do l <- spaces >> parser
                     _ <- spaces >> P.char '='
                     r <- spaces >> parser
                     return (binding # (l,r))

parseInherit :: CharParser st tok m Binding
parseInherit = do _ <- spaces >> P.string "inherit" >> lookAhead (P.space <|> P.char '(')
                  p <- option [] $ try $ between (spaces >> P.char '(')
                                                 (spaces >> P.char ')')
                                                 (spaces >> view path <$> parser)
                  i <- spaces >> parser
                  return (binding # (i, path # (p ++ [i])))

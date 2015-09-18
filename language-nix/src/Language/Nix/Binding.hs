{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Nix.Binding ( Binding, binding, localName, reference ) where

import Control.DeepSeq
import Control.Lens
import Data.Maybe
import Data.String
import Distribution.Compat.ReadP as ReadP
import Distribution.Text
import GHC.Generics ( Generic )
import Language.Nix.Identifier
import Language.Nix.Path
import Test.QuickCheck
import Text.PrettyPrint as PP

-- | A 'Binding' represents an identifier that refers to some other 'Path'.
--
-- >>> :set -XOverloadedStrings
-- >>> "inherit (foo.bar) abc" :: Binding
-- Bind (Identifier "abc") (Path [Identifier "foo",Identifier "bar",Identifier "abc"])
--
-- prop> \b -> Just (b :: Binding) == simpleParse (display b)

declareLenses [d| data Binding = Bind { localName :: Identifier, reference :: Path }
                    deriving (Show, Eq, Ord, Generic)
              |]

binding :: Iso' Binding (Identifier,Path)
binding = iso (\(Bind l r) -> (l,r)) (uncurry Bind)

instance NFData Binding where
  rnf (Bind l r) = l `deepseq` rnf r

instance Arbitrary Binding where
  arbitrary = review binding <$> arbitrary

instance Text Binding where
  disp b = case (init ps, last ps) of
             ([], i') -> if i == i'
                            then text "inherit" <+> disp i'
                            else disp i <+> equals <+> disp p
             (p', i') -> if i == i'
                            then text "inherit" <+> parens (disp (path # p')) <+> disp i'
                            else disp i <+> equals <+> disp p

        where
          (i, p) = view binding b
          ps = view path p

  parse = parseAssignment +++ parseInherit

instance IsString Binding where
  fromString s = fromMaybe (error ("invalid Nix binding: " ++ s)) (simpleParse s)

parseAssignment :: ReadP r Binding
parseAssignment = do l <- skipSpaces >> parse
                     _ <- skipSpaces >> ReadP.char '='
                     r <- skipSpaces >> parse
                     return (binding # (l,r))

parseInherit :: ReadP r Binding
parseInherit = do _ <- skipSpaces >> ReadP.string "inherit"
                  p <- option [] $ between (skipSpaces >> ReadP.char '(')
                                           (skipSpaces >> ReadP.char ')')
                                           (skipSpaces >> view path <$> parse)
                  i <- skipSpaces >> parse
                  return (binding # (i, path # (p ++ [i])))

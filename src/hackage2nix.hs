{-# LANGUAGE Rank2Types #-}

-- Run: cabal build -j hackage2nix && dist/build/hackage2nix/hackage2nix >hackage-packages.nix

module Main ( main ) where

import Cabal2Nix.Generate
import Cabal2Nix.Name
import Cabal2Nix.Package
import Control.Monad.IfElse
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Reader
import Control.Monad.State.Strict hiding ( State )
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set
import Distribution.Hackage.DB ( Hackage, readHackage, GenericPackageDescription )
import Distribution.NixOS.Derivation.Cabal
import Distribution.NixOS.PrettyPrinting hiding ( (<>) )
import Distribution.Text
import Options.Applicative
import System.IO

type Map = Map.Map

data Options = Options
  { verbose :: Bool
  }
  deriving (Show)

data Config = Config
  { _verbose :: Bool
  , _hackage :: Hackage
  }
  deriving (Show)

type Compile a = (Functor m, MonadIO m) => StateT () (ReaderT Config m) a

yell :: String -> Compile ()
yell = liftIO . hPutStrLn stderr

msgInfo, msgNote, msgWarn :: String -> Compile ()
msgInfo = whenM (asks _verbose) . yell . showString "INFO: "
msgNote = yell . showString "NOTE: "
msgWarn = yell . showString "WARN: "

runCompiler :: (Functor m, MonadIO m) => Compile a -> Options -> m a
runCompiler f opts = do
  db <- liftIO readHackage
  let cfg = Config
            { _verbose = verbose opts
            , _hackage = db
            }
      st  = ()
      f'  = msgInfo (showString "options = " (show opts)) >> f
  runReaderT (evalStateT f' st) cfg

-- | A convenience variant of 'runCompiler' that's probably good enough
-- for quick-fire testing in ghci.
run :: Compile a -> IO a
run f = runCompiler f Options { verbose = True }

main :: IO ()
main = execParser mainOptions >>= runCompiler buildPackageSet
  where
    parseOptions :: Parser Options
    parseOptions = Options
      <$> switch (long "verbose" <> help "enable detailed progress diagnostics")

    mainOptions :: ParserInfo Options
    mainOptions = info (helper <*> parseOptions)
      (  fullDesc
         <> header "hackage2nix -- convert the Hackage database into Nix build instructions"
      )

nixAttr :: String -> Version -> String
nixAttr name ver = show $ toNixName name -- ++ "_" ++ [ if c == '.' then '_' else c | c <- display ver ]

buildPackageSet :: Compile ()
buildPackageSet = do
  db <- fmap (\db -> foldr Map.delete db ["smtLib", "testPkg"]) (asks _hackage)
  pkgs <- liftIO $ runParIO $ parMapM generatePackage (Map.toList db)
  liftIO $ putStrLn "/* hackage-packages.nix is an auto-generated file -- DO NOT EDIT! */"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "{ stdenv, ghc, pkgs, definePackage }:"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "{"
  forM_ pkgs $ \(name, version, nixExpr) -> do
    liftIO $ print $ nest 2 $ hang (text (nixAttr name version) <+> equals <+> text "definePackage {}") 2 (parens (disp nixExpr)) <> semi
    liftIO $ putStrLn ""
  liftIO $ putStrLn "}"

generatePackage :: (String, Map Version GenericPackageDescription) -> ParIO (String,Version,Derivation)
generatePackage (name, versions) = do
  let latestVersion = Set.findMax (Map.keysSet versions)
      pkgDescription = (Map.!) versions latestVersion
  srcSpec <- liftIO $ sourceFromHackage Nothing (name ++ "-" ++ display latestVersion)
  return (name, latestVersion, (cabal2nix pkgDescription) { src = srcSpec })

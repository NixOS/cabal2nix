{-# LANGUAGE Rank2Types #-}

module Main ( main ) where

import Cabal2Nix.Generate
import Cabal2Nix.Name
import Cabal2Nix.Package
import Control.Monad.IfElse
import Control.Monad.Reader
import Control.Monad.State.Strict hiding ( State )
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set
import Distribution.Hackage.DB
import Distribution.NixOS.Derivation.Cabal
import Distribution.NixOS.PrettyPrinting hiding ( (<>) )
import Distribution.Text
import Options.Applicative
import System.IO

data Options = Options
  { verbose :: Bool
  }
  deriving (Show)

data Config = Config
  { _verbose :: Bool
  , _hackage :: Hackage
  }
  deriving (Show)

type Compile a = MonadIO m => StateT () (ReaderT Config m) a

yell :: String -> Compile ()
yell = liftIO . hPutStrLn stderr

msgInfo, msgNote, msgWarn :: String -> Compile ()
msgInfo = whenM (asks _verbose) . yell . showString "INFO: "
msgNote = yell . showString "NOTE: "
msgWarn = yell . showString "WARN: "

runCompiler :: MonadIO m => Compile a -> Options -> m a
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
nixAttr name ver = toNixName name ++ "_" ++ [ if c == '.' then '_' else c | c <- display ver ]

buildPackageSet :: Compile ()
buildPackageSet = do
  db <- asks _hackage
  forM_ (Map.toList db) $ \(name, versions) -> do
    let latestVersion = Set.findMax (Map.keysSet versions)
        pkgDescription = (Map.!) versions latestVersion
    srcSpec <- liftIO $ sourceFromHackage Nothing (name ++ "-" ++ display latestVersion)
    let nixExpr = (cabal2nix pkgDescription) { src = srcSpec }
    liftIO $ print $ hang (text (nixAttr name latestVersion) <+> equals) 2 (disp nixExpr) <> semi
    fail "yo"

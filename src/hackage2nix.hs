{-# LANGUAGE Rank2Types #-}

module Main where

import Control.DeepSeq
import Control.Monad.IfElse
import Control.Monad.Reader
import Control.Monad.State.Strict hiding ( State )
import Data.Char
import Data.List ( intercalate )
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compiler
import Distribution.Hackage.DB
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Text
import Distribution.Version
import Options.Applicative
import System.IO
import Text.PrettyPrint ( text )

data Options = Options
  { verbose :: Bool
  , overlay :: [FilePath]
  , compiler :: CompilerId
  }
  deriving (Show)

data Config = Config
  { _verbose :: Bool
  , _hackage :: Hackage
  , _recursionDepth :: Int
  , _compiler :: CompilerId
  }
  deriving (Show)

newtype Nixpkg = Nixpkg String
  deriving (Eq, Ord, Read, Show)

instance Text Nixpkg where
  disp (Nixpkg name) = text name
  parse = do
    ns <- Parse.sepBy1 component (Parse.char '-')
    return (Nixpkg (intercalate "-" ns))
      where
        -- Each component must contain an alphabetic character, to avoid
        -- ambiguity in identifiers like foo-1 (the 1 is the version number).
        component :: Parse.ReadP r String
        component = do
          cs <- Parse.munch1 isAlphaNum
          if all isDigit cs then Parse.pfail else return cs

data State = State (Map PackageName Version)
  deriving (Show)

type Compile a = MonadIO m => StateT State (ReaderT Config m) a

yell :: String -> Compile ()
yell msg = do
  d <- asks _recursionDepth
  let prefix = replicate d ' '
  liftIO (hPutStrLn stderr (prefix ++ msg))

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
            , _recursionDepth = 0
            , _compiler = compiler opts
            }
      st  = State Map.empty
      f'  = msgInfo (showString "options = " (show opts)) >> f
  runReaderT (evalStateT f' st) cfg

incDepth :: Compile a -> Compile a
incDepth f = local (\cfg -> cfg { _recursionDepth = 1 + _recursionDepth cfg }) f

-- | A convenience variant of 'runCompiler' that's probably good enough
-- for quick-fire testing in ghci.
run :: Compile a -> IO a
run f = runCompiler f Options
  { verbose = True, overlay = []
  , compiler = CompilerId GHC Version {versionBranch = [7,8,2], versionTags = []}
  }

main :: IO ()
main = execParser mainOptions >>= runCompiler buildPackageSet
  where
    parseCompilerId :: Parser CompilerId
    parseCompilerId = option (eitherReader (\s -> maybe (Left (show s ++ " is no valid compiler id")) Right (simpleParse s)))
                      (  long "compiler"
                      <> help "identifier of the compiler"
                      <> metavar "COMPILER-ID"
                      <> value (fromJust (simpleParse "ghc-7.8.2"))
                      <> showDefaultWith display
                      )

    parseOptions :: Parser Options
    parseOptions = Options
      <$> switch (long "verbose" <> help "enable detailed progress diagnostics")
      <*> many (strOption (long "overlay" <> help "path(s) to local Hackage overlay" <> metavar "PATH"))
      <*> parseCompilerId

    mainOptions :: ParserInfo Options
    mainOptions = info (helper <*> parseOptions)
      (  fullDesc
         <> header "hackage2nix -- convert the Hackage database into Nix build instructions"
      )

resolveName :: PackageName -> Compile (Map Version GenericPackageDescription)
resolveName (PackageName name) = do
  versions <- asks (Distribution.Hackage.DB.lookup name . _hackage)
  maybe (fail $ "resolve: unknown package " ++ show name) return versions

resolve :: Dependency -> Compile (Map Version GenericPackageDescription)
resolve dep@(Dependency pkgname@(PackageName name) versionRange) = do
  vdb <- resolveName pkgname
  msgInfo $ "resolve: " ++ name ++ " has versions: " ++ displayList (keys vdb)
  let matches = Distribution.Hackage.DB.filterWithKey (\k _ -> k `withinRange` versionRange) vdb
  if Distribution.Hackage.DB.null matches
    then fail $ "resolve: cannot satisfy " ++ display dep ++ " from versions " ++ displayList (keys vdb)
    else do msgInfo $ "resolve: " ++ display dep ++ " matches versions: " ++ displayList (keys matches)
            return matches

addPackage :: Dependency -> Compile ()
addPackage dep@(Dependency name _) = do
  msgInfo $ "try to register " ++ display dep
  known <- gets $ \(State pkgDb) -> dep `isKnownPackage` pkgDb
  unless known $ do
    vdb <- resolve dep
    r <- compile (last (elems vdb))
    case r of
      Left missingDeps -> incDepth $ do mapM_ addPackage (Prelude.filter (\(Dependency n _) -> n /= name) missingDeps)
                                        addPackage dep
      Right (pdesc, _) -> registerPackage (packageId pdesc)

isKnownPackage :: Dependency -> Map PackageName Version -> Bool
isKnownPackage (Dependency name versionRange) pkgDb =
  case Map.lookup name pkgDb of
    Nothing -> False
    Just v   -> v `withinRange` versionRange

compile :: GenericPackageDescription -> Compile (Either [Dependency] (PackageDescription, FlagAssignment))
compile gpdesc = do
  let PackageIdentifier name vers = packageId gpdesc
  knownPackages <- gets (\(State pkgDb) -> force $ Map.insert name vers pkgDb)
  let platformId = Platform X86_64 Linux
  compilerId <- asks _compiler
  return $ finalizePackageDescription
             ([] :: FlagAssignment)
             (`isKnownPackage` knownPackages)
             platformId
             compilerId
             [] -- [ thisPackageVersion (PackageIdentifier n v) | (n,v) <- Map.toList knownPackages ]
             gpdesc

registerPackage :: PackageIdentifier -> Compile ()
registerPackage pkgid@(PackageIdentifier name vers) = do
  msgNote $ "add " ++ display pkgid
  modify' $ \(State db) -> State (Map.insert name vers db)

buildPackageSet :: Compile ()
buildPackageSet = do
  mapM_ registerPackage corePackages
  mapM_ (addPackage . fromJust . simpleParse)
    [ "network < 2.5"           -- required because of hslogger
    -- here comes the actual payload
    , "cabal2nix"
    , "hledger"
    , "hledger-web"
    , "idris"
    , "Elm"
    , "Agda"
    , "git-annex"
    ]

  State pkgDb <- get
  mapM_ (liftIO . putStrLn . display . uncurry PackageIdentifier) (Map.toList pkgDb)

displayList :: Text a => [a] -> String
displayList = intercalate ", " . Prelude.map display

corePackages :: [PackageIdentifier]
corePackages = Prelude.map (fromJust . simpleParse)
               [ "Cabal-1.18.1.3"
               , "array-0.5.0.0"
               , "base-4.7.0.0"
               , "bin-package-db-0.0.0.0"
               , "binary-0.7.1.0"
               , "bytestring-0.10.4.0"
               , "containers-0.5.5.1"
               , "deepseq-1.3.0.2"
               , "directory-1.2.1.0"
               , "filepath-1.3.0.2"
               , "ghc-7.8.2"
               , "ghc-prim-0.3.1.0"
               , "haskell2010-1.1.2.0"
               , "haskell98-2.0.0.3"
               , "hoopl-3.10.0.1"
               , "hpc-0.6.0.1"
               , "integer-gmp-0.5.1.0"
               , "old-locale-1.0.0.6"
               , "old-time-1.1.0.2"
               , "pretty-1.1.1.1"
               , "process-1.2.0.0"
               , "rts-1.0"
               , "template-haskell-2.9.0.0"
               , "time-1.4.2"
               , "transformers-0.3.0.0"
               , "unix-2.7.0.1"
               ]

instance NFData State where
  rnf (State db) = rnf db

modify' :: (State -> State) -> Compile ()
modify' f = get >>= \st -> put $!! f st

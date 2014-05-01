{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Monad.IfElse
import Control.Monad.Reader
import Control.Monad.State hiding ( State )
import Data.Maybe
import Data.Monoid
import Distribution.Version
import Distribution.Compiler
import Distribution.Hackage.DB
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Text
import Options.Applicative
import System.IO
import Data.List ( reverse, intercalate )
import Text.PrettyPrint ( text )
import qualified Distribution.Compat.ReadP as Parse
import Data.Char

data Options = Options
  { verbose :: Bool
  , overlay :: [FilePath]
  , compiler :: CompilerId
  }
  deriving (Show)

data Config = Config
  { _verbose :: Bool
  , _hackage :: Hackage
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

data State = State (Map PackageIdentifier ())
  deriving (Show)

type Compile a = MonadIO m => StateT State (ReaderT Config m) a

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
            , _compiler = compiler opts
            }
      st  = State Distribution.Hackage.DB.empty
      f'  = msgInfo (showString "options = " (show opts)) >> f
  runReaderT (evalStateT f' st) cfg

-- | A convenience variant of 'runCompiler' that's probably good enough
-- for quick-fire testing in ghci.
run :: Compile a -> IO a
run f = runCompiler f Options
  { verbose = True, overlay = []
  , compiler = CompilerId GHC (Version {versionBranch = [7,8,2], versionTags = []})
  }

main :: IO ()
main = execParser mainOptions >>= runCompiler buildPackageSet
  where
    parseCompilerId :: Parser CompilerId
    parseCompilerId = nullOption
                      (    long "compiler"
                        <> eitherReader (\s -> maybe (Left (show s ++ " is no valid compiler id")) Right (simpleParse s))
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
  -- msgInfo $ "resolve: " ++ name ++ " has versions: " ++ displayList (keys vdb)
  let matches = Distribution.Hackage.DB.filterWithKey (\k _ -> k `withinRange` versionRange) vdb
  case Distribution.Hackage.DB.null matches of
    True  -> fail $ "resolve: cannot satisfy " ++ display dep ++ " from versions " ++ displayList (keys vdb)
    False -> do -- msgInfo $ "resolve: " ++ display dep ++ " matches versions: " ++ displayList (keys matches)
                return matches

addPackage :: Dependency -> Compile ()
addPackage dep = do
  known <- gets $ \(State pkgDb) -> dep `isKnownPackage` pkgDb
  case known of
    True -> do -- msgWarn (display dep ++ " is already registered!")
               return ()
    False -> do
      vdb <- resolve dep
      r <- compile (last (elems vdb))
      case r of
        Left missingDeps -> mapM_ addPackage missingDeps
        Right (pdesc, _) -> registerPackage (packageId pdesc)

isKnownPackage :: Dependency -> Map PackageIdentifier () -> Bool
isKnownPackage (Dependency (PackageName name) versionRange) pkgDb =
  not (Distribution.Hackage.DB.null (filterWithKey (\(PackageIdentifier (PackageName name') version) _ -> name' == name && version `withinRange` versionRange) pkgDb))

compile :: GenericPackageDescription -> Compile (Either [Dependency] (PackageDescription, FlagAssignment))
compile gpdesc = do
  State knownPackages <- get
  platformId <- return (Platform X86_64 Linux)
  compilerId <- asks _compiler
  return $ finalizePackageDescription
             ([] :: FlagAssignment)
             (`isKnownPackage` knownPackages)
             platformId
             compilerId
             (Prelude.map thisPackageVersion (keys knownPackages))
             gpdesc

registerPackage :: PackageIdentifier -> Compile ()
registerPackage pkgid = do
  msgInfo $ "registerPackage: " ++ display pkgid
  modify $ \(State db) -> State (insert pkgid () db)

buildPackageSet :: Compile ()
buildPackageSet = do
  mapM_ registerPackage corePackages
  addPackage (fromJust (simpleParse "hackage-db") :: Dependency)
  addPackage (fromJust (simpleParse "hledger") :: Dependency)
  -- addPackage (fromJust (simpleParse "git-annex") :: Dependency)

  -- Right (tpkg, _) = finalizePackageDescription
  --                       (configureCabalFlags pkg)
  --                       (const True)
  --                       (Platform I386 Linux)                   -- shouldn't be hardcoded
  --                       (CompilerId GHC (Version [7,6,3] []))   -- dito
  --                       [] cabal
  State pkgDb <- get
  mapM_ (msgNote . display) (keys pkgDb)


displayList :: Text a => [a] -> String
displayList = intercalate ", " . Prelude.map display

corePackages :: [PackageIdentifier]
corePackages = Prelude.map (fromJust . simpleParse)
               [ "ghc-prim-0.3.1"
               , "integer-simple-0.1.2"
               , "rts-1.0.1"
               ]

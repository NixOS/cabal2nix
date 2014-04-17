{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Monad.IfElse
import Control.Monad.Reader
import Control.Monad.State hiding ( State )
import Distribution.Hackage.DB
import Options.Applicative
import System.IO
import Data.Monoid
import Data.Maybe
import Distribution.Compiler
import Distribution.Text

data Options = Options
  { verbose :: Bool
  , overlay :: [FilePath]
  , compiler :: CompilerId
  }
  deriving (Show)

data Config = Config
  { _verbose :: Bool
  , _hackage :: Hackage
  }
  deriving (Show)

data State = State
  deriving (Show)

type Compile a = MonadIO m => StateT State (ReaderT Config m) a

yell :: String -> Compile ()
yell = liftIO . hPutStrLn stderr

msgInfo, msgWarn :: String -> Compile ()
msgInfo = whenM (asks _verbose) . yell . showString "INFO: "
msgWarn = yell . showString "WARN: "

runCompiler :: MonadIO m => Compile a -> Options -> m a
runCompiler f opts = do
  db <- liftIO readHackage
  let cfg = Config
            { _verbose = verbose opts
            , _hackage = db
            }
      st  = State
      f'  = msgInfo (showString "options = " (show opts)) >> f
  runReaderT (evalStateT f' st) cfg

main :: IO ()
main = execParser mainOptions >>= runCompiler (msgInfo "starting up")
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

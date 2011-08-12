module Main ( main ) where

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Control.Exception
import System.Directory
import System.FilePath
import Distribution.Simple.Program
import Data.List

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
       { hookedPrograms = [ bnfcProgram ]
       , hookedPreProcessors = [ ("cf", bnfc) ]
       }

bnfcProgram :: Program
bnfcProgram = (simpleProgram "BNFC")
  { programFindLocation = \v -> findProgramLocation v "bnfc"
  , programFindVersion = findProgramVersion "--numeric-version" id
  }

bnfc :: BuildInfo -> LocalBuildInfo -> PreProcessor
bnfc _ lbi = PreProcessor
  { platformIndependent = True
  , runPreProcessor = \(inBaseDir, inRelativeFile) (outBaseDir, outRelativeFile) verbosity -> do
      (bnfcProg,_,_) <- requireProgramVersion verbosity bnfcProgram (orLaterVersion (Version [2,4,2] [])) (withPrograms lbi)
      (happyProg,_,_) <- requireProgramVersion verbosity happyProgram (withinVersion (Version [1] [])) (withPrograms lbi)
      (alexProg,_,_) <- requireProgramVersion verbosity alexProgram (withinVersion (Version [2] [])) (withPrograms lbi)
      let back   = joinPath (replicate (length (splitDirectories outBaseDir)) "..")
          scope' = concat (intersperse "." (splitDirectories (takeDirectory outRelativeFile)))
          scope  = scope' ++ "." ++ takeFileName (dropExtension outRelativeFile)
      bracket (setCurrentDirectory outBaseDir) (\_ -> setCurrentDirectory back) $ \_ -> do
        rawSystemProgram verbosity bnfcProg
          [ "-haskell"
          , "-alex2"
          , "-d"
          , "-p", scope'
          , back </> inBaseDir </> inRelativeFile
          ]
        writeFile outRelativeFile $
          "module " ++ scope ++ "\n" ++
          "  (  module " ++ scope ++ ".Abs\n" ++
          "  ,  module " ++ scope ++ ".Lex\n" ++
          "  ,  module " ++ scope ++ ".Par\n" ++
          "  ,  module " ++ scope ++ ".Print\n" ++
          "  ,  module " ++ scope ++ ".ErrM\n" ++
          "  ,  ParseFun, run\n" ++
          "  )\n" ++
          "  where\n" ++
          "import " ++ scope ++ ".Abs\n" ++
          "import " ++ scope ++ ".Lex\n" ++
          "import " ++ scope ++ ".Par\n" ++
          "import " ++ scope ++ ".Print\n" ++
          "import " ++ scope ++ ".ErrM\n" ++
          "\n" ++
          "type ParseFun a = [Token] -> Err a\n" ++
          "\n" ++
          "run :: Monad m => ParseFun a -> String -> m a\n" ++
          "run p s = case p (myLexer s) of\n" ++
          "            Bad err    -> fail err\n" ++
          "            Ok  tree   -> return tree\n"
        rawSystemProgram verbosity happyProg
         [ "-gca"
         ,  dropExtension outRelativeFile </> "Par.y"
         ]
        rawSystemProgram verbosity alexProg
         [ "-g"
         ,  dropExtension outRelativeFile </> "Lex.x"
         ]
  }

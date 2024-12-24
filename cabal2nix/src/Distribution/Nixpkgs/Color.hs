module Distribution.Nixpkgs.Color
    ( maybeColor
    , colorStderrLn
    , infoColor
    , warningColor
    , errorColor
    , commandColor
    ) where

import System.Environment (lookupEnv)
import System.IO (Handle, hIsTerminalDevice, hPutStrLn, stderr)
import System.Console.ANSI.Codes
    ( setSGRCode
    , SGR(Reset, SetColor, SetConsoleIntensity)
    , ConsoleLayer(Foreground)
    , ColorIntensity(Vivid)
    , Color(Yellow, Red, Cyan)
    , ConsoleIntensity(BoldIntensity)
    )
import Control.Monad.IO.Class (MonadIO(liftIO))

-- | Colors that indicate a warning message.
warningColor :: [SGR]
warningColor = [SetColor Foreground Vivid Yellow]

infoColor :: [SGR]
infoColor = [SetColor Foreground Vivid Cyan]

-- | Colors that indicate an error message.
errorColor :: [SGR]
errorColor = [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]

-- | Colors that indicate a command is being executed.
commandColor :: [SGR]
commandColor = [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]

-- | Check if an environment variable is set and non-empty.
envIsSet :: String -> IO Bool
envIsSet name = do
    value <- lookupEnv name
    pure $ case value of
       Nothing -> False
       Just "" -> False
       Just _ -> True

-- | Should output to the given `Handle` be colored?
shouldColor :: Handle -> IO Bool
shouldColor handle = do
    -- See: https://no-color.org/
    noColor <- envIsSet "NO_COLOR"
    if noColor
       then pure False
       else do
           forceColor <- envIsSet "FORCE_COLOR"
           if forceColor
              then pure True
              else hIsTerminalDevice handle

-- | If the given `Handle` should be colored, wrap a `String` in `SGR` codes.
maybeColor :: Handle -> [SGR] -> String -> IO String
maybeColor handle sgrCodes original = do
    shouldColor' <- shouldColor handle
    if not shouldColor'
       then pure original
       else pure $ setSGRCode sgrCodes <> original <> setSGRCode [Reset]

colorStderrLn :: MonadIO m => [SGR] -> String -> m ()
colorStderrLn sgrCodes original = liftIO $ do
    maybeColored <- maybeColor stderr sgrCodes original
    hPutStrLn stderr maybeColored

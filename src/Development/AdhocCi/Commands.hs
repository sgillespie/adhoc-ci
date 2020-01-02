module Development.AdhocCi.Commands (runCommand) where

import Control.Exception (throw)
import System.Exit (ExitCode (..))
import System.IO (Handle, hGetBuffering, stderr, stdout)
import System.Process hiding (runCommand)

runCommand :: FilePath -> String -> IO ()
runCommand dir cmd = withCreateProcess' proc runCommand'
  where proc = (shell cmd) { cwd = Just dir,
                             std_out = UseHandle stdout,
                             std_err = UseHandle stderr
                           }

runCommand' :: ProcessHandle -> IO ()
runCommand' process = do
  res <- waitForProcess process
  case res of
    err@(ExitFailure _) -> throw err
    _                   -> return ()

withCreateProcess' :: CreateProcess -> (ProcessHandle -> IO a) -> IO a
withCreateProcess' p action = withCreateProcess p action'
  where action' _ _ _ = action

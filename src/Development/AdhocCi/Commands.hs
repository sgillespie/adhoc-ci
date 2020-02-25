{-# LANGUAGE DuplicateRecordFields #-}
module Development.AdhocCi.Commands
  (Command(..), MonadProcess(..), mkCommand) where

import System.Exit (ExitCode (..))
import System.IO (Handle, stderr, stdout)
import System.Process

data Command = Command
  { workDir :: FilePath,
    command :: String,
    stdout  :: Handle,
    stderr  :: Handle
  }
  deriving (Show, Eq)

mkCommand :: String -> Command
mkCommand cmd = Command
  { workDir = ".",
    command = cmd,
    stdout = System.IO.stdout,
    stderr = System.IO.stderr
  }

class Monad m => MonadProcess m where
  runCommand :: Command -> m ExitCode

instance MonadProcess IO where
  runCommand cmd = withCreateProcess' process waitForProcess
    where process = (shell cmd')
            { cwd = Just workDir',
              std_out = UseHandle stdout',
              std_err = UseHandle stderr'
            }

          Command
            { workDir = workDir',
              command = cmd',
              stdout = stdout',
              stderr = stderr'
            } = cmd

withCreateProcess'
  :: CreateProcess
  -> (ProcessHandle -> IO a)
  -> IO a
withCreateProcess' p action = withCreateProcess p action'
  where action' _ _ _ = action

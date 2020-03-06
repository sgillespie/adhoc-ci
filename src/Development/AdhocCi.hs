{-# LANGUAGE DuplicateRecordFields #-}
module Development.AdhocCi
  ( MonadPrint(..),
    PipelineOpts (..),
    mkPipelineOpts,
    runStages,
    module Development.AdhocCi.Commands,
    module Development.AdhocCi.Config
  ) where

import Control.Exception (throw)
import Control.Monad (forM_)
import Prelude hiding ((<$>), putStr, putStrLn)
import Data.Yaml (prettyPrintParseException)
import System.Exit (ExitCode (..))
import System.FilePath
import Text.PrettyPrint.ANSI.Leijen hiding ((</>), putDoc)
import qualified Prelude as P
import qualified System.Exit as E
import qualified Text.PrettyPrint.ANSI.Leijen as L

import Development.AdhocCi.Commands
import Development.AdhocCi.Config

data PipelineOpts = PipelineOpts
  { configFile :: FilePath,
    rootPath   :: FilePath,
    stage      :: [String]
  } deriving (Eq, Show)

mkPipelineOpts :: PipelineOpts
mkPipelineOpts = PipelineOpts
  { configFile = "adhoc-ci.yaml",
    rootPath   = ".",
    stage      = []
  }

class Monad m => MonadPrint m where
  putDoc :: Doc -> m ()
  putStr :: String -> m ()
  putStrLn :: String -> m ()
  exitWith :: ExitCode -> m a

instance MonadPrint IO where
  putDoc = L.putDoc
  putStr = P.putStr
  putStrLn = P.putStrLn
  exitWith = E.exitWith

runStages
  :: (MonadProcess m, MonadConfigFile m, MonadPrint m)
  => PipelineOpts
  -> m ()
runStages PipelineOpts{configFile=conf, rootPath=root, stage=stages'} = do
  let configPath = root </> conf

  config  <- parseConfigFile configPath
  config' <- case config of
    Left err  -> do
      putStr (prettyPrintParseException err)
      exitWith (ExitFailure 1)
    Right res -> return res

  let Hooks
        { beforeAll=beforeAll',
          beforeEach=beforeEach',
          afterAll=afterAll',
          afterEach=afterEach'
        } = hooks config'

  -- Run before_all hook
  runHook root beforeAll' $
    putDocLn $ boldYellow (text "Running before_all hook")
  
  -- Run the stages' jobs
  let cStages = filter (`elem` stages') (stages config')
  forM_ cStages $ \stage' -> do
    -- Run before_each hook
    runHook root beforeEach' $
      putDocLn $ boldYellow (text "Running before_each hook")

    -- Run the jobs
    putDocLn $ boldYellow (text "Running stage" <+> text stage')
    runStage root stage' (jobs config')

    -- Run after_each hook
    runHook root afterEach' $
      putDocLn $ boldYellow (text "Running after_each hook")

  -- Run after_all hook
  runHook root afterAll' $
    putDocLn $ boldYellow (text "Running after_all hook")

  putDocLn $ boldYellow (text "Job completed successfully")

runHook :: (MonadProcess m, MonadPrint m, MonadProcess m)
        => FilePath
        -> Maybe [String]
        -> m ()
        -> m ()
runHook dir (Just cmds) action = action >> mapM_ (runCommand' dir) cmds
runHook _ _ _                  = return ()

runStage :: (MonadProcess m, MonadPrint m) => FilePath -> String -> [Job] -> m ()
runStage dir stage' js
  = forM_ jobs' $ \ Job{commands=cs} ->
      forM_ cs (runCommand' dir)
  where jobs' = filter (\ Job{stage=s} -> s == stage') js

boldYellow :: Doc -> Doc
boldYellow = bold . yellow

putDocLn :: MonadPrint m => Doc -> m ()
putDocLn d = putDoc $ d <$> empty

runCommand' :: (MonadProcess m, MonadPrint m) => FilePath -> String -> m ()
runCommand' dir cmd = do
  let cmd' = (mkCommand cmd) { workDir = dir }

  putDocLn $ yellow (text "$" <+> text cmd)
  
  res <- runCommand cmd'
  case res of
    err@(ExitFailure _) -> throw err
    _                   -> return ()

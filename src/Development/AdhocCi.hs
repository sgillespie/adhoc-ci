{-# LANGUAGE DuplicateRecordFields #-}
module Development.AdhocCi
  ( PipelineOpts (..),
    mkPipelineOpts,
    runStages,
    module Development.AdhocCi.Commands,
    module Development.AdhocCi.Config
  ) where

import Control.Monad (forM_)
import Data.Yaml (prettyPrintParseException)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath

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

runStages :: PipelineOpts -> IO ()
runStages PipelineOpts{configFile=conf, rootPath=root, stage=stages'} = do
  let configPath = root </> conf

  config  <- parseConfigFile configPath
  config' <- case config of
    Left err  -> do
      putStr (prettyPrintParseException err)
      exitWith (ExitFailure 1)
    Right res -> return res

  let cStages = filter (`elem` stages') (stages config')

  forM_ cStages $ \stage' -> do
    putStrLn $ "Running stage " ++ stage'
    runStage root stage' (jobs config')

runStage :: FilePath -> String -> [Job] -> IO ()
runStage dir stage' js
  = forM_ jobs' $ \ Job{commands=c} ->
      forM_ c $ \command ->
        runCommand dir command
  where jobs' = filter (\ Job{stage=s} -> s == stage') js

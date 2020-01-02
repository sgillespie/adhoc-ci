module Development.AdhocCi
  ( PipelineOpts (..),
    mkPipelineOpts,
    runStages,
    module Development.AdhocCi.Commands,
    module Development.AdhocCi.Config
  ) where

import Control.Monad (forM_)
import qualified Data.ByteString as B
import Data.Yaml (encode, prettyPrintParseException)
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
    Left err      -> do
      putStr (prettyPrintParseException err)
      exitWith (ExitFailure 1)
    Right result -> return result

  let cStages = filter (\s -> name s `elem` stages') (stages config')

  forM_ cStages $ \ Stage{commands=cs, name=n} -> do
    putStrLn $ "Running stage " ++ n
    mapM_ (runCommand root) cs

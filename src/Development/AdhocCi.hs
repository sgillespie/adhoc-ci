module Development.AdhocCi (
  PipelineOpts (..),
  mkPipelineOpts,
  runStages,
  module Development.AdhocCi.Config
  ) where

import qualified Data.ByteString as B
import Data.Yaml (encode, prettyPrintParseException)
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
runStages opts = do
  result <- parseConfigFile configPath
  case result of
    Left err      -> putStr (prettyPrintParseException err)
    Right result' -> B.putStr (encode result')
  where configPath = rootPath opts ++ "/" ++ configFile opts

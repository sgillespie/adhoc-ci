module Development.AdhocCi where

-- pipeline:
--   stage1: ["cabal configure", "cabal build"]
--   stage2: ["cabal test"]
--   stage3: ["./deploy-app.sh"]

data Pipeline = Pipeline [Stage]
  deriving (Eq, Show)
data Stage = Stage String [String]
  deriving (Eq, Show)

data PipelineOpts = PipelineOpts
  { configFile :: FilePath,
    rootPath   :: FilePath,
    stage      :: String
  } deriving (Eq, Show)

mkPipelineOpts :: PipelineOpts
mkPipelineOpts = PipelineOpts
  { configFile = "adhoc-ci.yaml",
    rootPath   = ".",
    stage      = []
  }

build :: PipelineOpts -> IO ()
build opts = undefined

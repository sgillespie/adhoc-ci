module Main where

import Data.Semigroup ((<>))
import Development.AdhocCi
import Options.Applicative

main :: IO ()
main = customExecParser prefs' opts >>= run
  where prefs' = prefs (columns 100)
        opts = info (cmdParser <**> helper)
                    (fullDesc <> progDesc')
        progDesc' = progDesc "Execute a continuous integration pipeline"

run :: PipelineOpts -> IO ()
run = runStages

cmdParser :: Parser PipelineOpts
cmdParser
  = PipelineOpts <$> strOption configOpt
    <*> strOption rootOpt
    <*> many (strArgument pipeArg)
  where configOpt = long "configFile"
                    <> short 'f'
                    <> metavar "FILE"
                    <> value "adhoc-ci.yml"
                    <> help "location of the pipeline configuration file"
                    <> showDefault
                    <> hidden
        rootOpt   = long "root"
                    <> short 'd'
                    <> metavar "DIR"
                    <> value (rootPath mkPipelineOpts)
                    <> help "root directory of the project"
                    <> showDefault
                    <> hidden
        pipeArg   = metavar "STAGE..."
                  <> help "stage(s) to execute"

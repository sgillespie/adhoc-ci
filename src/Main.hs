module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import Options.Applicative.Help.Pretty (text)

data CmdOpts = CmdOpts {
  configFile :: FilePath,
  rootPath   :: FilePath,
  pipe       :: [String] }

defaultOpts :: CmdOpts
defaultOpts = CmdOpts {
  configFile = "adhoc-ci.yml",
  rootPath = ".",
  pipe = [] }

main :: IO ()
main = customExecParser prefs' opts >>= run
  where prefs' = prefs (columns 100)
        opts = info (dummyOpt <**> cmdParser <**> helper <**> dummyOpt)
                    (fullDesc <> progDesc')
        progDesc' = progDesc "Execute a continuous integration pipeline"
        dummyOpt = abortOption ShowHelpText $ mconcat
          [ long "help"
          , short 'h'
          , help "Show this help text"
          , style (const (text "asdf")) ]

run :: CmdOpts -> IO ()
run _ = putStrLn "Hello, Haskell!"

cmdParser :: Parser CmdOpts
cmdParser
  = CmdOpts <$> strOption configOpt
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
                    <> value (rootPath defaultOpts)
                    <> help "root directory of the project"
                    <> showDefault
                    <> hidden
        pipeArg   = metavar "PIPE..."
                  <> help "pipe(s) to execute"

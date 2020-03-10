{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Development.AdhocCiSpec (spec) where

import Control.Exception (throw)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Yaml (ParseException(..))
import System.Exit (ExitCode(..))
import Test.Hspec hiding (beforeAll, afterAll)
import Text.PrettyPrint.ANSI.Leijen (Doc(..))

import Development.AdhocCi

data FixtureInst m = FixtureInst
  { _runCommand :: Command -> m ExitCode,
    _parseConfigFile :: FilePath -> m (Either ParseException Config),
    _putDoc :: Doc -> m (),
    _putStr :: String -> m (),
    _putStrLn :: String -> m (),
    _exitWith :: forall a. ExitCode -> m a
  }

mkFixtureInst :: FixtureInst m
mkFixtureInst = FixtureInst
  { _runCommand = error "unimplemented instance method '_runCommand'",
    _parseConfigFile = error "unimplemented instance method '_parseConfigFile'",
    _putDoc = error "unimplemented instance method '_putDoc'",
    _putStr = error "unimplemented instance method '_putStr'",
    _putStrLn = error "unimplemented instance method '_putStrLn'",
    _exitWith = error "unimplemented instance method '_exitWith'"
  }

newtype TestM log a
  = TestM (ReaderT (FixtureInst (TestM log)) (Writer log) a)
  deriving (Functor,
            Applicative,
            Monad,
            MonadReader (FixtureInst (TestM log)),
            MonadWriter log
           )

logTestM :: FixtureInst (TestM l) -> TestM l a -> l
logTestM inst (TestM m) = execWriter (runReaderT m inst)

instance Monoid l => MonadProcess (TestM l) where
  runCommand c = do
    f <- asks _runCommand
    f c

instance Monoid l => MonadConfigFile (TestM l) where
  parseConfigFile file = do
    f <- asks _parseConfigFile
    f file

instance Monoid l => MonadPrint (TestM l) where
  putDoc d = do
    f <- asks _putDoc
    f d
  
  putStr s = do
    f <- asks _putStrLn
    f s

  putStrLn s = do
    f <- asks _putStr
    f s

  exitWith e = do
    f <- asks _exitWith
    f e

spec :: Spec
spec = describe "runStages" $ do
  let config = Config
        { stages = ["stage"],
          jobs = [Job "job" "stage" ["cmd"]],
          hooks = Hooks
            { beforeAll = Nothing,
              afterAll = Nothing,
              beforeEach = Nothing,
              afterEach = Nothing
            }
        }
  
  it "runs commands in specified stage" $ do
    let fixture = mkFixtureInst
          { _parseConfigFile = \_ -> return (Right config'),
            _runCommand = \cmd -> tell [cmd] >> return ExitSuccess,
            _putDoc = \_ -> return (),
            _putStrLn = \_ -> return (),
            _putStr = \_ -> return ()
          }
        config' = config { jobs = jobs config ++ [Job "job" "stage" ["cmd2"]] }
        opts = mkPipelineOpts { stage = ["stage"] :: [String] } :: PipelineOpts
        calls = logTestM fixture (runStages opts)
    calls `shouldBe`
      [mkCommand "cmd",
       mkCommand "cmd2"
      ]

  it "runs hooks" $ do
    let fixture = mkFixtureInst
          { _parseConfigFile = \_ -> return (Right config'),
            _runCommand = \cmd -> tell [cmd] >> return ExitSuccess,
            _putDoc = \_ -> return (),
            _putStrLn = \_ -> return (),
            _putStr = \_ -> return ()
          }
        config' = config
          { hooks = Hooks
              { beforeAll = Just ["before_all"],
                afterAll = Just ["after_all"],
                beforeEach = Just ["before_each"],
                afterEach = Just ["after_each"]
              }
          }
        opts = mkPipelineOpts { stage = ["stage"] :: [String] } :: PipelineOpts
        calls = logTestM fixture (runStages opts)
    calls `shouldBe`
      [ mkCommand "before_all",
        mkCommand "before_each",
        mkCommand "cmd",
        mkCommand "after_each",
        mkCommand "after_all"
      ]

  it "exits on parse failure" $ do
    let fixture = mkFixtureInst
          { _putStr = \_ -> return (),
            _putStrLn = \_ -> return (),
            _parseConfigFile = const $ return (Left (InvalidYaml Nothing)),
            _exitWith = \exit -> throw exit
          }
        opts = mkPipelineOpts
        calls = logTestM fixture (runStages opts) :: IO ()
    calls `shouldThrow` (== ExitFailure 1)

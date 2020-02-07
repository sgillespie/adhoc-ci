{-# LANGUAGE QuasiQuotes #-}
module Development.AdhocCi.ConfigSpec (spec) where

import Control.Exception (evaluate, throw)
import Data.Either (fromLeft, isRight)
import Data.Yaml
import Data.Yaml.TH (yamlQQ)
import Development.AdhocCi.Config
import System.Directory
import System.IO
import Test.Hspec

spec :: Spec
spec = describe "parseConfig" $ do
  let parseConfig' v = case parseConfig (encode v) of
                         Left e  -> throw e
                         Right c -> c

  it "parses a simple config" $ do
    let yaml = [yamlQQ|
                  stages:
                    - stage1
                    - stage2
                  job1:
                    stage: stage1
                    commands:
                      - build job1
                      - build2 job1
                  job2:
                    stage: stage2
                    commands:
                      - deploy job2
               |]
        config = parseConfig' yaml
    stages config `shouldBe` ["stage1", "stage2"]
    jobs config `shouldSatisfy` \jobs' ->
      Job "job1" "stage1" ["build job1", "build2 job1"] `elem` jobs' &&
      Job "job2" "stage2" ["deploy job2"] `elem` jobs'

  it "rejects non-objects" $ do
      let yaml =
            [[yamlQQ| ["a", "b", "c"] |],
             [yamlQQ| pipeline: ["a", "b", "c"] |]
            ]

      mapM_ (\y -> evaluate (parseConfig' y) `shouldThrow` anyException) yaml

  it "encodes to yaml" $ do
      let config = Config
            { stages = ["Stage1", "Stage2"],
              jobs   = [Job "Job1" "Stage1" ["command1", "command2"],
                        Job "Job2" "Stage2" ["command"]
                       ]
            }
          yaml = [yamlQQ|
                    stages: [Stage1, Stage2]
                    Job1:
                      stage: Stage1
                      commands: [command1, command2]
                    Job2:
                      stage: Stage2
                      commands: [command]
                  |]
      output <- decodeThrow $ showConfig config
      output `shouldBe` yaml

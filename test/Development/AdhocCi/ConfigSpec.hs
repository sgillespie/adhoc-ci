{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes           #-}
module Development.AdhocCi.ConfigSpec (spec) where

import Control.Exception (evaluate, throw)
import Data.Yaml
import Data.Yaml.TH (yamlQQ)
import Development.AdhocCi.Config
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
                  before_all:
                    - before command
                  before_each:
                    - before command 1
                  after_all:
                    - after command
                  after_each:
                    - after command 1
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
    hooks config `shouldBe`
      Hooks { beforeAll  = Just ["before command"],
              afterAll   = Just ["after command"],
              beforeEach = Just ["before command 1"],
              afterEach  = Just ["after command 1"]
            }

  it "defaults optional fields" $ do
    let yaml = [yamlQQ|
                 stages: []
               |]
        config = parseConfig' yaml
    hooks config `shouldBe`
      Hooks { beforeAll  = Nothing,
              afterAll   = Nothing,
              beforeEach = Nothing,
              afterEach  = Nothing
            }

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
                       ],
              hooks  = Hooks
                { beforeAll  = Nothing,
                  beforeEach = Nothing,
                  afterAll   = Nothing,
                  afterEach  = Nothing
                }
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


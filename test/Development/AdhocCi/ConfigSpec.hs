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
spec =
  describe "parseConfig" $ do
    let parseConfig' v = case parseConfig (encode v) of
                           Left e  -> throw e
                           Right c -> c

    it "parses a simple config" $ do
      let yaml = [yamlQQ|
                   pipeline:
                     - name: stage1
                       commands:
                         - cabal configure
                         - cabal test
                     - name: stage2
                       commands:
                         - ./deploy-app.sh
                 |]
      parseConfig' yaml `shouldBe`
        Config [
          Stage "stage1" ["cabal configure", "cabal test"],
          Stage "stage2" ["./deploy-app.sh"]
          ]

    it "rejects non-objects" $ do
      let yaml = [
            [yamlQQ| ["a", "b", "c"] |],
            [yamlQQ| pipeline: ["a", "b", "c"] |]
            ]

      mapM_ (\y -> evaluate (parseConfig' y) `shouldThrow` anyException) yaml


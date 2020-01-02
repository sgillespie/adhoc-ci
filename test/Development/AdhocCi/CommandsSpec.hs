module Development.AdhocCi.CommandsSpec (spec) where

import Development.AdhocCi.Commands
import System.Directory
import System.FilePath (takeFileName)
import System.IO
import Test.Hspec

spec :: Spec
spec =
  describe "runCommand" $ do
    it "runs a simple command" $ do
      (file, handle) <- openTempFile "/tmp" "runCommand"
      hClose handle

      exists <- doesFileExist file
      exists `shouldBe` True

      runCommand "/tmp" ("rm " ++ file)

      exists' <- doesFileExist file
      exists' `shouldBe` False

    it "runs in the specified working dir" $ do
      (file, handle) <- openTempFile "/tmp" "runCommand"
      hClose handle

      exists <- doesFileExist file
      exists `shouldBe` True

      runCommand "/tmp" ("rm " ++ takeFileName file)

      exists' <- doesFileExist file
      exists' `shouldBe` False

    it "fails on non-zero exit code" $
      runCommand "/tmp" "exit 1" `shouldThrow` anyException

    it "supports shell builtins" $
      runCommand "/tmp" "test -n $SHELL && test -z ''"

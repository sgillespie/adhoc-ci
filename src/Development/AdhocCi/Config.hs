{-# LANGUAGE OverloadedStrings #-}
module Development.AdhocCi.Config (
  parseConfig,
  parseConfigFile,
  Config(..),
  Stage(..)
  ) where

import Data.ByteString (ByteString)
import Data.Yaml (FromJSON (..), (.:))
import qualified Data.Yaml as Y

data Config
  = Config {
    stages :: [Stage]
  } deriving (Eq, Show)

data Stage
  = Stage {
      name     :: String,
      commands :: [String]
    } deriving (Eq, Show)

parseConfigFile :: FilePath -> IO (Either Y.ParseException Config)
parseConfigFile = Y.decodeFileEither

parseConfig :: ByteString -> Either Y.ParseException Config
parseConfig = Y.decodeEither'

instance FromJSON Config where
  parseJSON (Y.Object v) = Config <$> v .: "pipeline"
  parseJSON _            = fail "Expected an object"

instance FromJSON Stage where
  parseJSON (Y.Object v) = Stage <$>
    v .: "name" <*>
    v .: "commands"
  parseJSON _ = fail "Expected an object"

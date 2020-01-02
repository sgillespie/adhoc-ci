{-# LANGUAGE OverloadedStrings #-}
module Development.AdhocCi.Config (
  parseConfig,
  parseConfigFile,
  showConfig,
  Config(..),
  Stage(..)
  ) where

import Data.ByteString (ByteString)
import Data.Yaml (FromJSON (..), ToJSON, (.:), (.=))
import qualified Data.Yaml as Y

newtype Config
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

showConfig :: Config -> ByteString
showConfig = Y.encode

instance FromJSON Config where
  parseJSON (Y.Object v) = Config <$> v .: "pipeline"
  parseJSON _            = fail "Expected an object"

instance FromJSON Stage where
  parseJSON (Y.Object v) = Stage <$>
    v .: "name" <*>
    v .: "commands"
  parseJSON _ = fail "Expected an object"

instance ToJSON Config where
  toJSON Config{stages=stages} = Y.object [("pipeline", Y.array stages')]
    where stages' = map Y.toJSON stages

instance ToJSON Stage where
  toJSON Stage{name=name, commands=cmds}
    = Y.object [ "name" .= name,
                 "commands" .= cmds
               ]

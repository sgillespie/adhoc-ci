{-# LANGUAGE OverloadedStrings #-}
module Development.AdhocCi.Config (
  parseConfig,
  parseConfigFile,
  showConfig,
  Config(..),
  Job(..)
  ) where

import Control.Arrow ((>>>))
import Control.Monad (sequence)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (filterWithKey, insert, mapWithKey, toList)
import Data.Text (Text, pack)
import Data.Yaml (FromJSON (..), ToJSON, Value (..), object, (.:), (.=))
import qualified Data.Yaml as Y

data Config
  = Config {
    stages :: [String],
    jobs   :: [Job]
  } deriving (Eq, Show)

data Job
  = Job {
    name     :: String,
    stage    :: String,
    commands :: [String]
  } deriving (Eq, Show)

parseConfigFile :: FilePath -> IO (Either Y.ParseException Config)
parseConfigFile = Y.decodeFileEither

parseConfig :: ByteString -> Either Y.ParseException Config
parseConfig = Y.decodeEither'

showConfig :: Config -> ByteString
showConfig = Y.encode

reservedNames :: [Text]
reservedNames = ["stages"]

instance FromJSON Config where
  parseJSON (Y.Object v)
    = Config
      <$> v .: "stages"
      <*> parseJobs v
  parseJSON _            = fail "Expected an object"

parseJobs :: Y.Object -> Y.Parser [Job]
parseJobs = filterWithKey (const . (`notElem` reservedNames))
            >>> mapWithKey addName
            >>> toList
            >>> map (parseJSON . snd)
            >>> sequence
  where addName k (Y.Object v) = Object $ insert "name" (String k) v
        addName _ v            = v

instance FromJSON Job where
  parseJSON (Y.Object v)
    = Job
      <$> v .: "name"
      <*> v .: "stage"
      <*> v .: "commands"
  parseJSON _ = fail "Expected an object"

instance ToJSON Config where
  toJSON Config{stages=stages', jobs=jobs'}
    = Y.object (s : js)
    where s  = "stages" .= stages'
          js = map (\ j -> pack (name j) .= j) jobs'

instance ToJSON Job where
  toJSON Job{name=_, stage=st, commands=cmds}
    = object ["stage"    .= st,
              "commands" .= cmds
             ]

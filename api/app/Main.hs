module Main where

import Network.Wai.Logger
import Network.Wai.Handler.Warp
import Servant

import Development.AdhocCi.Api (Api, app, server)

main :: IO ()
main = withStdoutLogger $ \logger -> do
  let settings = setPort 8000 .
                 setLogger logger $ defaultSettings
  runSettings settings app

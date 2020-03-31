module Main where

import Network.Wai.Logger
import Network.Wai.Handler.Warp
import Servant

import Development.AdhocCi.Api (Api, server)

api :: Proxy Api
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = withStdoutLogger $ \logger -> do
  let settings = setPort 8000 .
                 setLogger logger $ defaultSettings
  runSettings settings app

module Development.AdhocCi.ServantClientSpec (spec) where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (Port(), testWithApplication)
import Servant.Client hiding (baseUrl, manager)
import Test.Hspec

import Development.AdhocCi.Api (api, app)

withUserApp :: (Port -> IO ()) -> IO ()
withUserApp action
  = testWithApplication (pure app) action

spec :: Spec
spec = around withUserApp $ do
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })
      ping = client api
  
  describe "GET /ping" $ do
    it "returns pong" $ \port -> do
      result <- runClientM ping (clientEnv port)
      result `shouldBe` Right "pong"

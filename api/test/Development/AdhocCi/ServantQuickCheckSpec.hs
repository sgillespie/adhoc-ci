module Development.AdhocCi.ServantQuickCheckSpec (spec) where

import Servant
import Servant.Client
import Servant.Server
import Servant.QuickCheck
import Servant.QuickCheck.Internal (serverDoesntSatisfy)
import Test.Hspec

import Development.AdhocCi.Api (api, server)

args :: Args
args = defaultArgs { maxSuccess = 500 }

ctx :: Context '[]
ctx = EmptyContext

spec :: Spec
spec = describe "Servant QuickCheck" $ do
  it "API demonstrates best practices" $
    withServantServerAndContext api ctx (return server) $ \burl ->
      serverSatisfies api burl args
                      (unauthorizedContainsWWWAuthenticate
                        <%> not500
                        <%> onlyJsonObjects  -- this one isn't true!
                        <%> mempty)

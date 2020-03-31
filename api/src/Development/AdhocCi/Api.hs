module Development.AdhocCi.Api where

import Servant

type Api = "api" :> "adhoc-ci" :> "v1" :> Api'

type Api' = "ping" :> Get '[PlainText] String

server :: Server Api
server = return "pong"

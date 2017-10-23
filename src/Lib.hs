module Lib
    ( startApp
    , app
    ) where

import Network.Wai.Handler.Warp
import Servant

import Souvenir.Link.API

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api linkV1

api :: Proxy LinkAPI
api = Proxy

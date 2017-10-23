module Lib
    ( startApp
    , app
    ) where

import Network.Wai.Handler.Warp
import Servant

import Souvenir.Link.API
import Souvenir.Link.Model

startApp :: IO ()
startApp = createDB >> run 8080 app

app :: Application
app = serve api linkV1

api :: Proxy LinkAPI
api = Proxy

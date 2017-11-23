{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Souvenir.Link.API where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Aeson.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Souvenir.Link.Fetch
import Souvenir.Link.Model as M

type LinkAPI =
    "users" :> Capture "userId" UserId :>
        ( Get '[JSON] [M.Link]
        :<|> ReqBody '[PlainText] M.URL :>
            Post '[JSON] (Maybe AdditionId)
        )

linkV1 :: Server LinkAPI
linkV1 userId =
    getUserLinksHandler userId
    :<|> saveLinkHandler userId

throwServant = Handler . throwE

userDo userId f = do
    exists <- liftIO $ userExists userId
    if not exists
       then throwServant err404
       else liftIO (f userId)

getUserLinksHandler userId = userDo userId getUserLinks

saveLinkHandler userId url =
    liftIO (fetchLink url) >>= \(Success link) ->
    userDo userId (`saveLink` link)

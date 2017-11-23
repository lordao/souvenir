{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Souvenir.Link.API where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Souvenir.Link.Model as M

type LinkAPI =
    "users" :> Capture "userId" UserId :>
        ( Get '[JSON] [M.Link]
        :<|> ReqBody '[JSON] M.Link :>
            Post '[JSON] AdditionId
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

saveLinkHandler userId link = userDo userId (`saveLink` link)

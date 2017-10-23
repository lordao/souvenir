{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Souvenir.Link.API where

import Control.Monad.IO.Class
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Souvenir.Link.Model as M

type LinkAPI =
    "links" :> Capture "userId" UserId :>
        ( Get '[JSON] [M.Link]
        :<|> ReqBody '[JSON] M.Link :>
            Post '[JSON] AdditionId
        )

linkV1 :: Server LinkAPI
linkV1 userId =
    liftIO (getUserLinks userId)
    :<|> liftIO . saveLink userId

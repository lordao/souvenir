{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Souvenir.Link.Model where

import Control.Monad
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Maybe
import Data.Time
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    email String
    deriving Eq Generic Show
Link
    url String
    UniqueUrl url
    deriving Eq Generic Show
Addition
    userId UserId
    linkId LinkId
    addedOn UTCTime
    UniqueAddition userId linkId
    deriving Eq Generic Show
|]

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Link)
$(deriveJSON defaultOptions ''Addition)

runDB = runSqlite "accounts.db"

createDB :: IO ()
createDB = runDB $ runMigration migrateAll

newUser :: User -> IO UserId
newUser = runDB . insert

getUserLinks :: UserId -> IO [Link]
getUserLinks = let getLink = get . additionLinkId . entityVal in
    userAdditions >=>
    runDB . mapM getLink >=>
    return . catMaybes

userAdditions :: UserId -> IO [Entity Addition]
userAdditions userId = runDB (selectList [AdditionUserId ==. userId] [])

saveLink :: UserId -> Link -> IO AdditionId
saveLink userId =
    getLinkKey >=> \linkId ->
    getCurrentTime >>=
    save . Addition userId linkId
    where save = runDB . insert

getLinkKey :: Link -> IO (Key Link)
getLinkKey link = findUrl url >>= maybe (addLink link) (return . entityKey)
    where url = linkUrl link

addLink = runDB . insert

findUrl = runDB . getBy . UniqueUrl

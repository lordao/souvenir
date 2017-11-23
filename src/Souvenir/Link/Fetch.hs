{-# LANGUAGE OverloadedStrings #-}
module Souvenir.Link.Fetch where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Network.HTTP.Simple
import Text.HTML.DOM
import Text.XML.Cursor
import Text.XML (Node)

import qualified Data.Text as T
import qualified Text.XML.Cursor.Generic as GC (Cursor)

import Souvenir.Link.Model

type NodeCursor = GC.Cursor Node
type Metadata = [(Text, NodeCursor -> [Text])]

getCursor url = fmap fromDocument (httpSink url $ const sinkDoc)

fetchLink :: (MonadIO m, MonadMask m) => URL -> m (Result Link)
fetchLink url = fmap extractLink $ parseRequest url >>= getCursor
    where
        extractLink = fromJSON . makeObject . findMetadata linkMetadata
        makeObject = object . ("linkUrl" .= url :)

findMetadata :: Metadata -> NodeCursor -> [Pair]
findMetadata metadata cursor = uncurry (.=) . fmap (T.concat . (cursor $//)) <$> metadata

linkMetadata :: Metadata
linkMetadata =
    [ ("linkTitle", element "title" &/ content)
    , ("linkDescription", element "meta" >=> attributeIs "name" "description" >=> attribute "content")
    , ("linkImage", element "meta" >=> attributeIs "property" "og:image" >=> attribute "content")
    ]

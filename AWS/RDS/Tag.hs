{-# LANGUAGE FlexibleContexts #-}

module AWS.RDS.Tag
    ( listTagsForResource
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Conduit
import Data.Text (Text)
import Data.XML.Types (Event)

import AWS.Lib.Parser
import AWS.Lib.Query
import AWS.RDS.Internal
import AWS.RDS.Types (Tag(..))

listTagsForResource
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ ResourceName
    -> RDS m [Tag]
listTagsForResource name =
    rdsQuery "ListTagsForResource" params $
        elements' "TagList" "Tag" tagSink
  where
    params =
        [ "ResourceName" |= name
        ]

tagSink
    :: MonadThrow m
    => Consumer Event m Tag
tagSink = Tag
    <$> getT "Value"
    <*> getT "Key"

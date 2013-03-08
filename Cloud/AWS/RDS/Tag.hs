{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.RDS.Tag
    ( listTagsForResource
    , addTagsToResource
    , removeTagsFromResource
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Conduit
import Data.Text (Text)
import Data.XML.Types (Event)

import Cloud.AWS.Lib.Parser
import Cloud.AWS.Lib.Query
import Cloud.AWS.RDS.Internal
import Cloud.AWS.RDS.Types (Tag(..))

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

addTagsToResource
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ ResourceName
    -> [Tag] -- ^ Tags
    -> RDS m ()
addTagsToResource name tags =
    rdsQueryOnlyMetadata "AddTagsToResource" params
  where
    params =
        [ "ResourceName" |= name
        , "Tags.member" |.#. map tagParams tags
        ]
    tagParams tag =
        [ "Value" |= tagValue tag
        , "Key" |= tagKey tag
        ]

removeTagsFromResource
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ ResourceName
    -> [Text] -- ^ TagKeys
    -> RDS m ()
removeTagsFromResource name keys =
    rdsQueryOnlyMetadata "RemoveTagsFromResource" params
  where
    params =
        [ "ResourceName" |= name
        , "TagKeys.member" |.#= keys
        ]

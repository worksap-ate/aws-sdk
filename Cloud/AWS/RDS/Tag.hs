{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.RDS.Tag
    ( listTagsForResource
    , addTagsToResource
    , removeTagsFromResource
    ) where

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<))
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
import Data.Text (Text)

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
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m Tag
tagSink xml = Tag
    <$> xml .< "Value"
    <*> xml .< "Key"

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

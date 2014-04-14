{-# LANGUAGE FlexibleContexts, CPP #-}

module AWS.EC2.Tag
    ( describeTags
    , createTags
    , deleteTags
    ) where

import Data.Text (Text)
import Control.Applicative
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadBaseControl, MonadResource)
import Data.Conduit (ResumableSource)
#else
import Data.Conduit
#endif

import AWS.EC2.Internal
import AWS.EC2.Types
import AWS.EC2.Query
import AWS.Lib.Parser

describeTags
    :: (MonadResource m, MonadBaseControl IO m)
    => [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m Tag)
describeTags filters =
    ec2QuerySource "DescribeTags" params $ itemConduit "tagSet" $
        Tag
        <$> getT "resourceId"
        <*> getT "resourceType"
        <*> getT "key"
        <*> getT "value"
  where
    params = [filtersParam filters]

createTags
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ ResourceId (instance-id, image-id,..)
    -> [(Text, Text)] -- ^ (Key, Value)
    -> EC2 m Bool
createTags rids kvs =
    ec2Query "CreateTags" params $ getT "return"
  where
    params =
        [ "ResourceId" |.#= rids
        , "Tag" |.#. map tagParams kvs
        ]
    tagParams (k, v) =
        [ "Key" |= k
        , "Value" |= v
        ]

deleteTags
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ ResourceId (instance-id, image-id,..)
    -> [ResourceTag]
    -> EC2 m Bool
deleteTags rids tags =
    ec2Query "DeleteTags" params $ getT "return"
  where
    params =
        [ "ResourceId" |.#= rids
        , "Tag" |.#. map tagParams tags
        ]
    tagParams tag =
        [ "Key" |= resourceTagKey tag
        , "Value" |=? resourceTagValue tag
        ]

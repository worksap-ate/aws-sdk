{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Tag
    ( describeTags
    , createTags
    , deleteTags
    ) where

import Data.Text (Text)
import Data.Monoid
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Types
import AWS.EC2.Class
import AWS.EC2.Query
import AWS.EC2.Parser
import AWS.Util

describeTags
    :: (MonadResource m, MonadBaseControl IO m)
    => [Filter] -- ^ Filters
    -> EC2 m (Source m Tag)
describeTags filters =
    ec2QuerySource "DescribeTags" params $ itemConduit "tagSet" $
        Tag
        <$> getT "resourceId"
        <*> getT "resourceType"
        <*> getT "key"
        <*> getMT "value"
  where
    params = [FilterParams filters]

createTags
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ ResourceId (instance-id, image-id,..)
    -> [(Text, Text)]
    -> EC2 m Bool
createTags rids kvs =
    ec2Query "CreateTags" params returnBool
  where
    params =
        [ ArrayParams "ResourceId" rids ]
        ++ concatMap (uncurry tags) (zip ([1..]::[Int]) kvs)
    tags n (k, v) =
        [ ValueParam ("Tag." <> toText n <> ".Key") k
        , ValueParam ("Tag." <> toText n <> ".Value") v
        ]

deleteTags
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ ResourceId (instance-id, image-id,..)
    -> [ResourceTag]
    -> EC2 m Bool
deleteTags rids tags =
    ec2Query "DeleteTags" params returnBool
  where
    params =
        [ ArrayParams "ResourceId" rids ]
        ++ concatMap (uncurry tagParam) (zip ([1..]::[Int]) tags)
    tagParam n tag =
        [ ValueParam ("Tag." <> toText n <> ".Key")
         $ resourceKey tag ]
        ++ maybe []
            (\a -> [ValueParam ("Tag." <> toText n <> ".Value") a])
            (resourceValue tag)

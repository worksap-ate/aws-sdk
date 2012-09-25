{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Tag
    ( describeTags
    ) where

import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Types
import AWS.EC2.Class
import AWS.EC2.Query
import AWS.EC2.Parser

describeTags
    :: (MonadResource m, MonadBaseControl IO m)
    => [Filter] -- ^ Filters
    -> EC2 m (Source m Tag)
describeTags filters =
    ec2QuerySource "DescribeTags" params $ itemConduit "tagSet" $
        tag
        <$> getT "resourceId"
        <*> getT "resourceType"
        <*> getT "key"
        <*> getMT "value"
  where
    params = [FilterParams filters]

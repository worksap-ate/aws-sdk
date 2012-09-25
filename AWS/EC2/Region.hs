{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Region
    ( describeRegions
    ) where

import Data.Text (Text)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Types
import AWS.EC2.Class
import AWS.EC2.Query
import AWS.EC2.Parser

describeRegions
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ RegionNames
    -> [Filter] -- ^ Filters
    -> EC2 m (Source m Region)
describeRegions regions filters =
    ec2QuerySource "DescribeRegions" params regionInfoConduit
  where
    params =
        [ ArrayParams "RegionName" regions
        , FilterParams filters
        ]
    regionInfoConduit :: MonadThrow m
        => GLConduit Event m Region
    regionInfoConduit = itemConduit "regionInfo" $
        region
        <$> getT "regionName"
        <*> getT "regionEndpoint"

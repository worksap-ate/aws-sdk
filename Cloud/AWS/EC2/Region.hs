{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.EC2.Region
    ( describeRegions
    ) where

import Data.Text (Text)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Applicative

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query
import Cloud.AWS.Lib.Parser

describeRegions
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ RegionNames
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m Region)
describeRegions regions filters =
    ec2QuerySource "DescribeRegions" params regionInfoConduit
  where
    params =
        [ "RegionName" |.#= regions
        , filtersParam filters
        ]
    regionInfoConduit :: MonadThrow m
        => Conduit Event m Region
    regionInfoConduit = itemConduit "regionInfo" $
        Region
        <$> getT "regionName"
        <*> getT "regionEndpoint"

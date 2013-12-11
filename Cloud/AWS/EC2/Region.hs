{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.EC2.Region
    ( describeRegions
    ) where

import Data.Text (Text)

import Data.Conduit
import Control.Applicative

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query
import Cloud.AWS.Lib.Parser.Unordered

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
    regionInfoConduit = itemConduit "regionInfo" $ \xml ->
        Region
        <$> xml .< "regionName"
        <*> xml .< "regionEndpoint"

{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.EC2.Region
    ( describeRegions
    ) where

import Data.Text (Text)

import Data.Conduit
import Control.Applicative
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)

import Cloud.AWS.Lib.Parser.Unordered ((.<))

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query

describeRegions
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ RegionNames
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m Region)
describeRegions regions filters =
    ec2QuerySource "DescribeRegions" params path regionInfoConduit
  where
    path = itemsPath "regionInfo"
    params =
        [ "RegionName" |.#= regions
        , filtersParam filters
        ]
    regionInfoConduit = itemConduit $ \e -> Region
        <$> e .< "regionName"
        <*> e .< "regionEndpoint"

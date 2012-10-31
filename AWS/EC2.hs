{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module AWS.EC2
    ( -- * EC2 Environment
      EC2
    , runEC2
    , setRegion
      -- * Instances
    , module AWS.EC2.Instance
      -- * Images
    , module AWS.EC2.Image
      -- * Volumes
    , module AWS.EC2.Volume
      -- * Snapshots
    , module AWS.EC2.Snapshot
      -- * Addresses
    , module AWS.EC2.Address
      -- * KeyPairs
    , module AWS.EC2.KeyPair
      -- * SecurityGroups
    , module AWS.EC2.SecurityGroup
      -- * Placements
    , module AWS.EC2.Region
    , module AWS.EC2.AvailabilityZone
      -- * Tags
    , module AWS.EC2.Tag
      -- * VPC
    , module AWS.EC2.VPC
    , module AWS.EC2.Subnets
    , module AWS.EC2.Acl
    , module AWS.EC2.RouteTable
    ) where

import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.State as State
import Data.Text (Text)

import AWS.Util
import AWS.Class
import AWS.EC2.Internal
import AWS.EC2.Types
import qualified AWS.EC2.Util as Util

import AWS.EC2.Image
import AWS.EC2.Region
import AWS.EC2.AvailabilityZone
import AWS.EC2.Instance
import AWS.EC2.Address
import AWS.EC2.Tag
import AWS.EC2.Snapshot
import AWS.EC2.Volume
import AWS.EC2.KeyPair
import AWS.EC2.SecurityGroup
import AWS.EC2.VPC
import AWS.EC2.Subnets
import AWS.EC2.Acl
import AWS.EC2.RouteTable

-- | set endpoint to EC2 context.
setRegion
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ RegionName
    -> EC2 m ()
setRegion name = do
    region <- Util.head $ describeRegions [name] []
    ctx <- State.get
    maybe
        (fail "region not found")
        (\r -> State.put ctx { endpoint = f r })
        region
  where
    f = textToBS . regionEndpoint

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Cloud.AWS.EC2
    ( -- * EC2 Environment
      EC2
    , runEC2
    , runEC2withManager
    , setRegion
    , setEndpoint
    , apiVersion
      -- * Instances
    , module Cloud.AWS.EC2.Instance
      -- * Images
    , module Cloud.AWS.EC2.Image
      -- * Volumes
    , module Cloud.AWS.EC2.Volume
      -- * Snapshots
    , module Cloud.AWS.EC2.Snapshot
      -- * Addresses
    , module Cloud.AWS.EC2.Address
      -- * KeyPairs
    , module Cloud.AWS.EC2.KeyPair
       -- * SecurityGroups
    , module Cloud.AWS.EC2.SecurityGroup
      -- * NetworkInterface
    , module Cloud.AWS.EC2.NetworkInterface
    , module Cloud.AWS.EC2.NetworkInterfaceAttribute
      -- * Placements
    , module Cloud.AWS.EC2.Region
    , module Cloud.AWS.EC2.AvailabilityZone
    , module Cloud.AWS.EC2.PlacementGroup
      -- * Tags
    , module Cloud.AWS.EC2.Tag
      -- * VPC
    , module Cloud.AWS.EC2.VPC
    , module Cloud.AWS.EC2.Subnets
    , module Cloud.AWS.EC2.Acl
    , module Cloud.AWS.EC2.RouteTable
    , module Cloud.AWS.EC2.Route
      -- * Tasks
    , module Cloud.AWS.EC2.ConversionTask
    ) where

import qualified Control.Monad.State as State
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Data.Text (Text)
import Data.ByteString (ByteString)

import Cloud.AWS.Class
import Cloud.AWS.Lib.Query (textToBS)
import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import qualified Cloud.AWS.EC2.Util as Util
import Cloud.AWS.EC2.Query (apiVersion)

import Cloud.AWS.EC2.Image
import Cloud.AWS.EC2.Region
import Cloud.AWS.EC2.AvailabilityZone
import Cloud.AWS.EC2.Instance
import Cloud.AWS.EC2.Address
import Cloud.AWS.EC2.Tag
import Cloud.AWS.EC2.Snapshot
import Cloud.AWS.EC2.Volume
import Cloud.AWS.EC2.KeyPair
import Cloud.AWS.EC2.SecurityGroup
import Cloud.AWS.EC2.VPC
import Cloud.AWS.EC2.Subnets
import Cloud.AWS.EC2.Acl
import Cloud.AWS.EC2.Route
import Cloud.AWS.EC2.RouteTable
import Cloud.AWS.EC2.NetworkInterface
import Cloud.AWS.EC2.NetworkInterfaceAttribute
import Cloud.AWS.EC2.PlacementGroup
import Cloud.AWS.EC2.ConversionTask

-- | set endpoint to EC2 context by giving the EC2 region.
setRegion
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ RegionName
    -> EC2 m ()
setRegion name = do
    region <- Util.head $ describeRegions [name] []
    maybe (fail "region not found") (setEndpoint . g) region
  where
    g = textToBS . regionEndpoint

-- | set endpoint to EC2 context.
setEndpoint :: (MonadResource m, MonadBaseControl IO m)
    => ByteString -- ^ ec2 endpoint domain <http://docs.amazonwebservices.com/general/latest/gr/rande.html>
    -> EC2 m ()
setEndpoint ep = do
    ctx <- State.get
    State.put ctx { endpoint = ep }

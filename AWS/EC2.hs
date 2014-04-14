{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}

module AWS.EC2
    ( -- * EC2 Environment
      EC2
    , runEC2
    , runEC2withManager
    , setRegion
    , setEndpoint
    , apiVersion
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
      -- * NetworkInterface
    , module AWS.EC2.NetworkInterface
    , module AWS.EC2.NetworkInterfaceAttribute
      -- * Placements
    , module AWS.EC2.Region
    , module AWS.EC2.AvailabilityZone
    , module AWS.EC2.PlacementGroup
      -- * Tags
    , module AWS.EC2.Tag
      -- * VPC
    , module AWS.EC2.VPC
    , module AWS.EC2.Subnets
    , module AWS.EC2.Acl
    , module AWS.EC2.RouteTable
    , module AWS.EC2.Route
      -- * Tasks
    , module AWS.EC2.ConversionTask
    ) where

#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadBaseControl, MonadResource)
#else
import Data.Conduit
#endif
import qualified Control.Monad.State as State
import Data.Text (Text)
import Data.ByteString (ByteString)

import AWS.Class
import AWS.Lib.Query (textToBS)
import AWS.EC2.Internal
import AWS.EC2.Types
import qualified AWS.EC2.Util as Util
import AWS.EC2.Query (apiVersion)

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
import AWS.EC2.Route
import AWS.EC2.RouteTable
import AWS.EC2.NetworkInterface
import AWS.EC2.NetworkInterfaceAttribute
import AWS.EC2.PlacementGroup
import AWS.EC2.ConversionTask

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

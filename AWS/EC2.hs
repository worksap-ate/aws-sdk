{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module AWS.EC2
    ( -- * EC2 Environment
      module AWS.EC2.Class
    , EC2Endpoint(..)
    , setEndpoint
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
    ) where

import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.State as State

import AWS.Types
import AWS.EC2.Class

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

setEndpoint
    :: (MonadResource m, MonadBaseControl IO m)
    => EC2Endpoint -> EC2 m ()
setEndpoint ep = do
    ctx <- State.get
    State.put ctx { endpoint = ep }

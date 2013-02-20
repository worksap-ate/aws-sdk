{-# LANGUAGE TemplateHaskell #-}

module AWS.EC2.Types.Subnets
    ( CreateSubnetRequest(..)
    , Subnet(..)
    , SubnetState
    ) where

import AWS.EC2.Types.Common (ResourceTag)
import AWS.Lib.FromText

data CreateSubnetRequest = CreateSubnetRequest
    { createSubnetRequestVpcId :: Text
    , createSubnetRequestCidrBlock :: AddrRange IPv4
    , createSubnetRequestAvailabilityZone :: Maybe Text
    }
  deriving (Show, Read, Eq)

data Subnet = Subnet
    { subnetId :: Text
    , subnetState :: SubnetState
    , subnetVpcId :: Text
    , subnetCidrBlock :: AddrRange IPv4
    , subnetAvailableIpAddressCount :: Int
    , subnetAvailabilityZone :: Text
    , subnetDefaultForAz :: Maybe Bool
    , subnetMapPublicIpOnLaunch :: Maybe Bool
    , subnetTagSet :: [ResourceTag]
    }
  deriving (Show, Read, Eq)

data SubnetState
    = SubnetStatePending
    | SubnetStateAvailable
  deriving (Show, Read, Eq)

deriveFromText "SubnetState" ["pending", "available"]

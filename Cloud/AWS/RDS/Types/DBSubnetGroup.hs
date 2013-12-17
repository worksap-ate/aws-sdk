module Cloud.AWS.RDS.Types.DBSubnetGroup
    ( DBSubnetGroup(..)
    , Subnet(..)
    , AvailabilityZone(..)
    ) where

import Data.Text (Text)

data DBSubnetGroup = DBSubnetGroup
    { dbSubnetGroupVpcId :: Text
    , dbSubnetGroupStatus :: Text
    , dbSubnetGroupDescription :: Text
    , dbSubnetGroupName :: Text
    , dbSubnets :: [Subnet]
    }
  deriving (Show, Eq)

data Subnet = Subnet
    { subnetStatus :: Text
    , subnetIdentifier :: Text
    , subnetAvailabilityZone :: AvailabilityZone
    }
  deriving (Show, Eq)

data AvailabilityZone = AvailabilityZone
    { availabilityZoneName :: Text
    , availabilityZoneProvisionedIopsCapable :: Bool
    }
  deriving (Show, Eq)

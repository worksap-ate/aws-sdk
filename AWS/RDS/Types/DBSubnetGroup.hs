module AWS.RDS.Types.DBSubnetGroup
    ( DBSubnetGroup(..)
    , Subnet(..)
    , AvailabilityZone(..)
    ) where

import AWS.Lib.FromText (Text)

data DBSubnetGroup = DBSubnetGroup
    { dbsngVpcId :: Text
    , dbsngSubnetGroupStatus :: Text
    , dbsngDBSubnetGroupDescription :: Text
    , dbsngDBSubnetGroupName :: Text
    , dbsngSubnets :: [Subnet]
    }
  deriving (Show, Eq)

data Subnet = Subnet
    { subnetStatus :: Text
    , subnetIdentifier :: Text
    , subnetAvailabilityZone :: AvailabilityZone
    }
  deriving (Show, Eq)

data AvailabilityZone = AvailabilityZone
    { azName :: Text
    , azProvisionedIopsCapable :: Bool
    }
  deriving (Show, Eq)

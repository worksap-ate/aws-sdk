{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.EC2.Types.Address
    ( Address(..)
    , AddressDomain(..)
    , AllocateAddress(..)
    , AssociateAddressRequest(..)
    , DisassociateAddressRequest(..)
    ) where

import Cloud.AWS.Lib.FromText (FromText(..), failText)
import Data.IP (IPv4)
import Data.Text (Text)

data Address = Address
    { addressPublicIp :: IPv4
    , addressAllocationId :: Maybe Text
    , addressDomain :: AddressDomain
    , addressInstanceId :: Maybe Text
    , addressAssociationId :: Maybe Text
    , addressNetworkInterfaceId :: Maybe Text
    , addressNetworkInterfaceOwnerId :: Maybe Text
    , addressPrivateIpAddress :: Maybe IPv4
    }
  deriving (Show, Read, Eq)

data AddressDomain
    = AddressDomainStandard
    | AddressDomainVPC
  deriving (Show, Read, Eq)

instance FromText AddressDomain where
    fromText t
        | t == "standard" = return AddressDomainStandard
        | t == "vpc"      = return AddressDomainVPC
        | otherwise       = fail "unknown AddressDomain"
    fromNamedText _name Nothing  = return AddressDomainStandard
    fromNamedText _name (Just t) = maybe (failText t) return $ fromText t

data AllocateAddress = AllocateAddress
    { allocateAddressPublicIp :: IPv4
    , allocateAddressDomain :: AddressDomain
    , allocateAddressAllocationId :: Maybe Text
    }
  deriving (Show, Read, Eq)

data AssociateAddressRequest
    = AssociateAddressRequestEc2
        { associateAddressRequestEc2PublicIp :: IPv4
        , associateAddressRequestEc2InstanceId :: Text
        }
    | AssociateAddressRequestVpc
        { associateAddressRequestVpcAllocationId :: Text
        , associateAddressRequestVpcInstanceId :: Maybe Text
        , associateAddressRequestVpcNetworkInterfaceId
            :: Maybe Text
        , associateAddressRequestVpcPrivateIpAddress :: Maybe IPv4
        , associateAddressRequestVpcAllowReassociation
            :: Maybe Bool
        }
  deriving (Show, Read, Eq)

data DisassociateAddressRequest
    = DisassociateAddressRequestEc2 IPv4 -- ^ PublicIp for EC2
    | DisassociateAddressRequestVpc Text -- ^ AssociationId for VPC
      -- ^ AssociationId for VPC
  deriving (Show, Read, Eq)

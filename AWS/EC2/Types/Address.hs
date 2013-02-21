{-# LANGUAGE TemplateHaskell #-}

module AWS.EC2.Types.Address
    ( Address(..)
    , AddressDomain(..)
    , AllocateAddress(..)
    , AssociateAddressRequest(..)
    , DisassociateAddressRequest(..)
    ) where

import AWS.Lib.FromText

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

instance FromText AddressDomain
  where
    fromMaybeText _name Nothing  = return AddressDomainStandard
    fromMaybeText _name (Just t)
        | t == "standard" = return AddressDomainStandard
        | t == "vpc"      = return AddressDomainVPC
        | otherwise       = monadThrow $ FromTextError t

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

{-# LANGUAGE FlexibleContexts, CPP #-}

module AWS.EC2.Address
    ( describeAddresses
    , allocateAddress
    , releaseAddress
    , associateAddress
    , AssociateAddressRequest(..)
    , disassociateAddress
    , DisassociateAddressRequest(..)
    ) where

import Data.Text (Text)
import Data.IP (IPv4)
import Data.XML.Types (Event)
import Data.Conduit
import Control.Applicative
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadBaseControl, MonadResource)
#endif

import AWS.EC2.Internal
import AWS.EC2.Types
import AWS.EC2.Query
import AWS.Lib.Parser
import AWS.Util

-----------------------------------------------------
-- DescribeAddresses
-----------------------------------------------------
describeAddresses
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ PublicIps
    -> [Text] -- ^ AllocationIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m Address)
describeAddresses pubIps alloIds filters =
    ec2QuerySource "DescribeAddresses" params addressSet where
    params =
        [ "PublicIp" |.#= pubIps
        , "AllocationId" |.#= alloIds
        , filtersParam filters
        ]

    addressSet :: MonadThrow m => Conduit Event m Address
    addressSet = itemConduit "addressesSet" $ Address
        <$> getT "publicIp"
        <*> getT "allocationId"
        <*> getT "domain"
        <*> getT "instanceId"
        <*> getT "associationId"
        <*> getT "networkInterfaceId"
        <*> getT "networkInterfaceOwnerId"
        <*> getT "privateIpAddress"

-----------------------------------------------------
-- AllocateAddress
-----------------------------------------------------
allocateAddress
    :: (MonadResource m, MonadBaseControl IO m)
    => Bool -- ^ is VPC?
    -> EC2 m AllocateAddress
allocateAddress isVpc = do
    ec2Query "AllocateAddress" params $
        AllocateAddress
        <$> getT "publicIp"
        <*> getT "domain"
        <*> getT "allocationId"
  where
    params = if isVpc then ["Domain" |= "vpc"] else []

-----------------------------------------------------
-- ReleaseAddress
-----------------------------------------------------
releaseAddress
    :: (MonadResource m, MonadBaseControl IO m)
    => Maybe IPv4 -- ^ PublicIp
    -> Maybe Text -- ^ AllocationId
    -> EC2 m EC2Return
releaseAddress addr allocid = do
    ec2Query "ReleaseAddress" params $ getT "return"
  where
    params =
        [ "PublicIp" |=? toText <$> addr
        , "AllocationId" |=? allocid
        ]

-----------------------------------------------------
-- AssociateAddress
-----------------------------------------------------
associateAddress
    :: (MonadResource m, MonadBaseControl IO m)
    => AssociateAddressRequest
    -> EC2 m (Bool, Maybe Text)
associateAddress param = ec2Query "AssociateAddress" params $
    (,) <$> getT "return"
        <*> getT "associationId"
  where
    params = associateAddressParams param

associateAddressParams
    :: AssociateAddressRequest -> [QueryParam]
associateAddressParams (AssociateAddressRequestEc2 ip iid) =
    [ "PublicIp" |= toText ip
    , "InstanceId" |= iid
    ]
associateAddressParams (AssociateAddressRequestVpc aid iid nid pip ar) =
    [ "AllocationId" |= aid
    , "InstanceId" |=? iid
    , "NetworkInterfaceId" |=? nid
    , "PrivateIpAddress" |=? toText <$> pip
    , "AllowReassociation" |=? boolToText <$> ar
    ]

disassociateAddress
    :: (MonadResource m, MonadBaseControl IO m)
    => DisassociateAddressRequest
    -> EC2 m Bool
disassociateAddress param =
    ec2Query "DisassociateAddress" (p param)
        $ getT "return"
  where
    p (DisassociateAddressRequestEc2 pip)
        = ["PublicIp" |= toText pip]
    p (DisassociateAddressRequestVpc aid)
        = ["AssociationId" |= aid]

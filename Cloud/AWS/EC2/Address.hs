{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.EC2.Address
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
import Data.Conduit
import Control.Applicative
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)

import Cloud.AWS.Lib.Parser.Unordered ((.<))

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query

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
    ec2QuerySource "DescribeAddresses" params path addressSet
  where
    params =
        [ "PublicIp" |.#= pubIps
        , "AllocationId" |.#= alloIds
        , filtersParam filters
        ]
    path = itemsPath "addressesSet"
    addressSet = itemConduit $ \xml ->
        Address
        <$> xml .< "publicIp"
        <*> xml .< "allocationId"
        <*> xml .< "domain"
        <*> xml .< "instanceId"
        <*> xml .< "associationId"
        <*> xml .< "networkInterfaceId"
        <*> xml .< "networkInterfaceOwnerId"
        <*> xml .< "privateIpAddress"

-----------------------------------------------------
-- AllocateAddress
-----------------------------------------------------
allocateAddress
    :: (MonadResource m, MonadBaseControl IO m)
    => Bool -- ^ is VPC?
    -> EC2 m AllocateAddress
allocateAddress isVpc = do
    ec2Query "AllocateAddress" params $ \xml ->
        AllocateAddress
        <$> xml .< "publicIp"
        <*> xml .< "domain"
        <*> xml .< "allocationId"
  where
    params = if isVpc then ["Domain" |= ("vpc" :: Text)] else []

-----------------------------------------------------
-- ReleaseAddress
-----------------------------------------------------
releaseAddress
    :: (MonadResource m, MonadBaseControl IO m)
    => Maybe IPv4 -- ^ PublicIp
    -> Maybe Text -- ^ AllocationId
    -> EC2 m EC2Return
releaseAddress addr allocid = do
    ec2Query "ReleaseAddress" params (.< "return")
  where
    params =
        [ "PublicIp" |=? addr
        , "AllocationId" |=? allocid
        ]

-----------------------------------------------------
-- AssociateAddress
-----------------------------------------------------
associateAddress
    :: (MonadResource m, MonadBaseControl IO m)
    => AssociateAddressRequest
    -> EC2 m (Bool, Maybe Text)
associateAddress param = ec2Query "AssociateAddress" params $ \xml ->
    (,) <$> xml .< "return"
        <*> xml .< "associationId"
  where
    params = associateAddressParams param

associateAddressParams
    :: AssociateAddressRequest -> [QueryParam]
associateAddressParams (AssociateAddressRequestEc2 ip iid) =
    [ "PublicIp" |= ip
    , "InstanceId" |= iid
    ]
associateAddressParams (AssociateAddressRequestVpc aid iid nid pip ar) =
    [ "AllocationId" |= aid
    , "InstanceId" |=? iid
    , "NetworkInterfaceId" |=? nid
    , "PrivateIpAddress" |=? pip
    , "AllowReassociation" |=? ar
    ]

disassociateAddress
    :: (MonadResource m, MonadBaseControl IO m)
    => DisassociateAddressRequest
    -> EC2 m Bool
disassociateAddress param =
    ec2Query "DisassociateAddress" (p param) (.< "return")
  where
    p (DisassociateAddressRequestEc2 pip)
        = ["PublicIp" |= pip]
    p (DisassociateAddressRequestVpc aid)
        = ["AssociationId" |= aid]

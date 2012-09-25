{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Address
    ( describeAddresses
    , allocateAddress
    , releaseAddress
    ) where

import Data.Text (Text)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Types
import AWS.EC2.Class
import AWS.EC2.Query
import AWS.EC2.Parser

-----------------------------------------------------
-- DescribeAddresses
-----------------------------------------------------
describeAddresses
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ PublicIps
    -> [Text] -- ^ AllocationIds
    -> [Filter] -- ^ Filters
    -> EC2 m (Source m Address)
describeAddresses pubIps alloIds filters =
    ec2QuerySource "DescribeAddresses" params addressSet where
    params =
        [ ArrayParams "PublicIp" pubIps
        , ArrayParams "AllocationId" alloIds
        , FilterParams filters
        ]

    addressSet :: MonadThrow m => GLConduit Event m Address
    addressSet = itemConduit "addressesSet" $ address
        <$> getT "publicIp"
        <*> getMT "allocationId"
        <*> getM "domain" addressDomain
        <*> getMT "instanceId"
        <*> getMT "associationId"
        <*> getMT "networkInterfaceId"
        <*> getMT "networkInterfaceOwnerId"
        <*> getMT "privateIpAddress"

-----------------------------------------------------
-- AllocateAddress
-----------------------------------------------------
allocateAddress
    :: (MonadResource m, MonadBaseControl IO m)
    => Bool -- ^ is VPC?
    -> EC2 m AllocateAddressResponse
allocateAddress isVpc = do
    ec2Query "AllocateAddress" params $ do
        yield =<< allocateAddressResponse
            <$> getT "publicIp"
            <*> getM "domain" addressDomain
            <*> getMT "allocationId"
  where
    params = if isVpc then [ValueParam "Domain" "vpc"] else []

-----------------------------------------------------
-- ReleaseAddress
-----------------------------------------------------
releaseAddress
    :: (MonadResource m, MonadBaseControl IO m)
    => Maybe Text -- ^ PublicIp
    -> Maybe Text -- ^ AllocationId
    -> EC2 m EC2Return
releaseAddress addr allocid = do
    ec2Query "ReleaseAddress" params $ do
        yield =<< getF "return" ec2Return
  where
    param name = maybe [] (\a -> [ValueParam name a])
    params = uncurry param =<<
        [ ("PublicIp", addr)
        , ("AllocationId", allocid)
        ]

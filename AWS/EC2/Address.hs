{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Address
    ( describeAddresses
    , allocateAddress
    , releaseAddress
    ) where

import Data.Text (Text)

import Data.XML.Types (Event)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Class (lift)
import Control.Applicative

import AWS.EC2.Types
import AWS.EC2.Query
import AWS.EC2.Parser

-----------------------------------------------------
-- DescribeAddresses
-----------------------------------------------------
describeAddresses
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text]
    -> [Text] -> [Filter]
    -> EC2 m (Source m Address)
describeAddresses pubIps alloIds filters =
    ec2Query "DescribeAddresses" params addressSet where
    params =
        [ ArrayParams "PublicIp" pubIps
        , ArrayParams "AllocationId" alloIds
        , FilterParams filters
        ]

    addressSet :: MonadThrow m => GLConduit Event m Address
    addressSet = itemConduit "addressesSet" $ address
        <$> getT "publicIp"
        <*> getMT "allocationId"
        <*> getF "domain" (const AddressDomainStandard)
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
    => Bool
    -> EC2 m AllocateAddressResponse
allocateAddress isVpc = do
    src <- ec2Query "AllocateAddress" params $ do
        yield =<< allocateAddressResponse
            <$> getT "publicIp"
            <*> getM "domain" (const AddressDomainStandard)
            <*> getMT "allocationId"
    lift (src $$ CL.head) >>= maybe (fail "") return
  where
    params = if isVpc then [ValueParam "Domain" "vpc"] else []

-----------------------------------------------------
-- ReleaseAddress
-----------------------------------------------------
releaseAddress
    :: (MonadResource m, MonadBaseControl IO m)
    => Maybe Text
    -> Maybe Text
    -> EC2 m EC2Return
releaseAddress addr allocid = do
    src <- ec2Query "ReleaseAddress" params $ do
        yield =<< getF "return" ec2Return
    lift (src $$ CL.head) >>= maybe (fail "") return
  where
    param name = maybe [] (\a -> [ValueParam name a])
    params = uncurry param =<<
        [ ("PublicIp", addr)
        , ("AllocationId", allocid)
        ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Address
    ( describeAddresses
    ) where

import           Data.ByteString (ByteString)

import Data.XML.Types
import Data.Conduit
import Control.Monad.Trans.Control
import Control.Applicative

import AWS.EC2.Types
import AWS.EC2.Query
import AWS.EC2.Parser

{----------------------------------------------------
 - DescribeAddresses
 ---------------------------------------------------}
describeAddresses
    :: (MonadResource m, MonadBaseControl IO m)
    => [ByteString]
    -> [ByteString]
    -> [Filter]
    -> EC2 m (EC2Response (Source m Address))
describeAddresses pubIps alloIds filters =
    ec2Query "DescribeAddresses" params addressSet
  where
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

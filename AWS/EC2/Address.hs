{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Address
    ( describeAddresses
    , allocateAddress
    , releaseAddress
    , associateAddress
    , AssociateAddressParam(..)
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
import AWS.Util

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
    ec2Query "AllocateAddress" params $
        allocateAddressResponse
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
    ec2Query "ReleaseAddress" params $ getF "return" ec2Return
  where
    param name = maybe [] (\a -> [ValueParam name a])
    params = uncurry param =<<
        [ ("PublicIp", addr)
        , ("AllocationId", allocid)
        ]

-----------------------------------------------------
-- AssociateAddress
-----------------------------------------------------
associateAddress
    :: (MonadResource m, MonadBaseControl IO m)
    => AssociateAddressParam
    -> EC2 m (Bool, Maybe Text)
associateAddress param = ec2Query "AssociateAddress" params $
    (,) <$> getF "return" textToBool
        <*> getMT "associationId"
  where
    params = associateAddressParam param

data AssociateAddressParam
    = AAEC2Instance
        { aaec2PublicIp :: Text
        , aaec2InstanceId :: Text
        }
    | AAVPCInstance
        { aavpcAllocationId :: Text
        , aavpcInstanceId :: Maybe Text
        , aavpcNetworkInterfaceId :: Maybe Text
        , aavpcPrivateIpAddress :: Maybe Text
        , aavpcAllowReassociation :: Maybe Bool
        }
  deriving (Show)

associateAddressParam
    :: AssociateAddressParam -> [QueryParam]
associateAddressParam (AAEC2Instance ip iid) =
    [ ValueParam "PublicIp" ip
    , ValueParam "InstanceId" iid
    ]
associateAddressParam (AAVPCInstance aid iid nid pip ar) =
    [ ValueParam "AllocationId" aid ]
    ++ (uncurry f =<<
        [ ("InstanceId", iid)
        , ("NetworkInterfaceId", nid)
        , ("PrivateIpAddress", pip)
        , ("AllowReassociation", boolToText <$> ar)
        ])
  where
    f name = maybe [] (\a -> [ValueParam name a])

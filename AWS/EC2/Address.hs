{-# LANGUAGE FlexibleContexts #-}

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

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

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
        [ ArrayParams "PublicIp" pubIps
        , ArrayParams "AllocationId" alloIds
        , FilterParams filters
        ]

    addressSet :: MonadThrow m => GLConduit Event m Address
    addressSet = itemConduit "addressesSet" $ Address
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
        AllocateAddressResponse
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
    params = maybeParams
        [ ("PublicIp", addr)
        , ("AllocationId", allocid)
        ]

-----------------------------------------------------
-- AssociateAddress
-----------------------------------------------------
associateAddress
    :: (MonadResource m, MonadBaseControl IO m)
    => AssociateAddressRequest
    -> EC2 m (Bool, Maybe Text)
associateAddress param = ec2Query "AssociateAddress" params $
    (,) <$> getF "return" textToBool
        <*> getMT "associationId"
  where
    params = associateAddressParam param

associateAddressParam
    :: AssociateAddressRequest -> [QueryParam]
associateAddressParam (AAEC2Instance ip iid) =
    [ ValueParam "PublicIp" ip
    , ValueParam "InstanceId" iid
    ]
associateAddressParam (AAVPCInstance aid iid nid pip ar) =
    [ ValueParam "AllocationId" aid ]
    ++ maybeParams
        [ ("InstanceId", iid)
        , ("NetworkInterfaceId", nid)
        , ("PrivateIpAddress", pip)
        , ("AllowReassociation", boolToText <$> ar)
        ]

disassociateAddress
    :: (MonadResource m, MonadBaseControl IO m)
    => DisassociateAddressRequest
    -> EC2 m Bool
disassociateAddress param =
    ec2Query "DisassociateAddress" (p param)
        $ getF "return" textToBool
  where
    p (DAEC2 pip) = [ValueParam "PublicIp" pip]
    p (DAVPC aid) = [ValueParam "AssociationId" aid]

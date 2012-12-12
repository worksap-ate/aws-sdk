{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.EC2.NetworkInterface
    ( assignPrivateIpAddresses
    , unassignPrivateIpAddresses
    , describeNetworkInterfaces
    ) where

import Data.IP (IPv4)
import Data.Text (Text)
import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Convert
import AWS.EC2.Internal
import AWS.EC2.Types
import AWS.EC2.Params
import AWS.EC2.Query
import AWS.Lib.Parser
import AWS.Util

assignPrivateIpAddresses
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ NetworkInterfaceId
    -> Either [IPv4] Int -- ^ PrivateIpAddresses or Count
    -> Maybe Bool
    -> EC2 m Bool
assignPrivateIpAddresses niid epip ar =
    ec2Query "AssignPrivateIpAddresses" params returnBool
  where
    params = [ValueParam "NetworkInterfaceId" niid]
        ++ either f g epip
        ++ maybeParams [("AllowReassignment", boolToText <$> ar)]
    f addrs = [privateIpAddressesParam "PrivateIpAddress" addrs]
    g cnt = [ValueParam "SecondaryPrivateIpAddressCount"
        $ toText cnt]

unassignPrivateIpAddresses
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ NetworkInterfaceId
    -> [IPv4] -- ^ PrivateIpAddresses
    -> EC2 m Bool
unassignPrivateIpAddresses niid addrs =
    ec2Query "UnassignPrivateIpAddresses" params returnBool
  where
    params =
        [ ValueParam "NetworkInterfaceId" niid
        , privateIpAddressesParam "PrivateIpAddress" addrs
        ]

describeNetworkInterfaces
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ NetworkInterfaceIds
    -> [Filter]
    -> EC2 m (ResumableSource m NetworkInterface)
describeNetworkInterfaces niid filters =
    ec2QuerySource "DescribeNetworkInterfaces" params
        $ itemConduit "networkInterfaceSet" networkInterfaceSink
  where
    params =
        [ ArrayParams "NetworkInterfaceId" niid
        , FilterParams filters
        ]

networkInterfaceSink
    :: MonadThrow m
    => GLSink Event m NetworkInterface
networkInterfaceSink = NetworkInterface
    <$> getT "networkInterfaceId"
    <*> getT "subnetId"
    <*> getT "vpcId"
    <*> getT "availabilityZone"
    <*> getT "description"
    <*> getT "ownerId"
    <*> getMT "requesterId"
    <*> getT "requesterManaged"
    <*> getF "status" networkInterfaceStatus'
    <*> getT "macAddress"
    <*> getT "privateIpAddress"
    <*> getMT "privateDnsName"
    <*> getF "sourceDestCheck" textToBool
    <*> groupSetSink
    <*> elementM "attachment" (NetworkInterfaceAttachment
        <$> getT "attachmentId"
        <*> getMT "instanceId"
        <*> getT "instanceOwnerId"
        <*> getT "deviceIndex"
        <*> getT "status"
        <*> getF "attachTime" textToTime
        <*> getF "deleteOnTermination" textToBool
        )
    <*> networkInterfaceAssociationSink
    <*> resourceTagSink
    <*> itemsSet "privateIpAddressesSet" (
        NetworkInterfacePrivateIpAddress
        <$> getT "privateIpAddress"
        <*> getF "primary" textToBool
        <*> networkInterfaceAssociationSink
        )

networkInterfaceAssociationSink
    :: MonadThrow m
    => GLSink Event m (Maybe NetworkInterfaceAssociation)
networkInterfaceAssociationSink =
    elementM "association" $ NetworkInterfaceAssociation
        <$> getMT "attachmentId"
        <*> getMT "instanceId"
        <*> getT "publicIp"
        <*> getT "ipOwnerId"
        <*> getT "associationId"

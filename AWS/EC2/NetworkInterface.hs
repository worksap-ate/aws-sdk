{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}

module AWS.EC2.NetworkInterface
    ( assignPrivateIpAddresses
    , unassignPrivateIpAddresses
    , describeNetworkInterfaces
    , createNetworkInterface
    , deleteNetworkInterface
    , attachNetworkInterface
    , detachNetworkInterface
    ) where

import Data.IP (IPv4)
import Data.Text (Text)
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

assignPrivateIpAddresses
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ NetworkInterfaceId
    -> Either [IPv4] Int -- ^ PrivateIpAddresses or Count
    -> Maybe Bool
    -> EC2 m Bool
assignPrivateIpAddresses niid epip ar =
    ec2Query "AssignPrivateIpAddresses" params $ getT "return"
  where
    params =
        [ "NetworkInterfaceId" |= niid
        , either f g epip
        , "AllowReassignment" |=? boolToText <$> ar
        ]
    f = ("PrivateIpAddress" |.#=) . map toText
    g = ("SecondaryPrivateIpAddressCount" |=) . toText

unassignPrivateIpAddresses
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ NetworkInterfaceId
    -> [IPv4] -- ^ PrivateIpAddresses
    -> EC2 m Bool
unassignPrivateIpAddresses niid addrs =
    ec2Query "UnassignPrivateIpAddresses" params $ getT "return"
  where
    params =
        [ "NetworkInterfaceId" |= niid
        , "PrivateIpAddress" |.#= map toText addrs
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
        [ "NetworkInterfaceId" |.#= niid
        , filtersParam filters
        ]

networkInterfaceSink
    :: MonadThrow m
    => Consumer Event m NetworkInterface
networkInterfaceSink = NetworkInterface
    <$> getT "networkInterfaceId"
    <*> getT "subnetId"
    <*> getT "vpcId"
    <*> getT "availabilityZone"
    <*> getT "description"
    <*> getT "ownerId"
    <*> getT "requesterId"
    <*> getT "requesterManaged"
    <*> getT "status"
    <*> getT "macAddress"
    <*> getT "privateIpAddress"
    <*> getT "privateDnsName"
    <*> getT "sourceDestCheck"
    <*> groupSetSink
    <*> networkInterfaceAttachmentSink
    <*> networkInterfaceAssociationSink
    <*> resourceTagSink
    <*> itemsSet "privateIpAddressesSet" (
        NetworkInterfacePrivateIpAddress
        <$> getT "privateIpAddress"
        <*> getT "privateDnsName"
        <*> getT "primary"
        <*> networkInterfaceAssociationSink
        )

networkInterfaceAssociationSink
    :: MonadThrow m
    => Consumer Event m (Maybe NetworkInterfaceAssociation)
networkInterfaceAssociationSink =
    elementM "association" $ NetworkInterfaceAssociation
        <$> getT "attachmentId"
        <*> getT "instanceId"
        <*> getT "publicIp"
        <*> getT "publicDnsName"
        <*> getT "ipOwnerId"
        <*> getT "associationId"

createNetworkInterface
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the subnet to associate with the network interface.
    -> SecondaryPrivateIpAddressParam -- ^ The private IP address of the specified network interface.
    -> Maybe Text -- ^ The description of the network interface.
    -> [Text] -- ^ A list of security group IDs for use by the network interface.
    -> EC2 m NetworkInterface
createNetworkInterface subnet privateAddresses description securityGroupIds =
    ec2Query "CreateNetworkInterface" params $ element "networkInterface" networkInterfaceSink
  where
    params :: [QueryParam]
    params =
        [ "SubnetId" |= subnet
        , "Description" |=? description
        , "SecurityGroup" |.#= securityGroupIds
        ] ++ fromSecondary privateAddresses

    fromSecondary :: SecondaryPrivateIpAddressParam -> [QueryParam]
    fromSecondary SecondaryPrivateIpAddressParamNothing = []
    fromSecondary (SecondaryPrivateIpAddressParamCount n) = ["SecondaryPrivateIpAddressCount" |= toText n]
    fromSecondary (SecondaryPrivateIpAddressParamSpecified addrs primary) =
        [ "PrivateIpAddresses" |.#. map (\addr -> ["PrivateIpAddress" |= toText addr]) addrs
        , maybeParam $ primaryParam <$> primary
        ]

    primaryParam :: Int -> QueryParam
    primaryParam n = "PrivateIpAddresses" |.+ toText n |.+ "Primary" |= "true"

deleteNetworkInterface
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface.
    -> EC2 m Bool
deleteNetworkInterface networkInterface =
    ec2Query "DeleteNetworkInterface" ["NetworkInterfaceId" |= networkInterface] $ getT "return"

attachNetworkInterface
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface to attach.
    -> Text -- ^ The ID of the instance to attach to the network interface.
    -> Int -- ^ The index of the device for the network interface attachment.
    -> EC2 m Text -- ^ The ID of the attachment.
attachNetworkInterface networkInterface inst deviceIdx =
    ec2Query "AttachNetworkInterface" params $ getT "attachmentId"
  where
    params =
        [ "NetworkInterfaceId" |= networkInterface
        , "InstanceId" |= inst
        , "DeviceIndex" |= toText deviceIdx
        ]

detachNetworkInterface
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the attachment.
    -> Maybe Bool -- ^ Set to true to force a detachment.
    -> EC2 m Bool
detachNetworkInterface attachment force =
    ec2Query "DetachNetworkInterface" params $ getT "return"
  where
    params =
        [ "AttachmentId" |= attachment
        , "Force" |=? toText <$> force
        ]

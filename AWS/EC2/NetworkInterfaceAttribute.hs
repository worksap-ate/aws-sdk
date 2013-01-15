{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module AWS.EC2.NetworkInterfaceAttribute
   ( describeNetworkInterfaceDescription
   , describeNetworkInterfaceGroupSet
   , describeNetworkInterfaceSourceDestCheck
   , describeNetworkInterfaceAttachment
   ) where

import Data.Text (Text)
import Data.Conduit (GLSink, MonadBaseControl, MonadResource)
import Data.XML.Types (Event)

import AWS.EC2.Internal (EC2, groupSetSink, networkInterfaceAttachmentSink)
import AWS.EC2.Query (ec2Query, (|=))
import AWS.EC2.Types (Group, NetworkInterfaceAttachment(..))
import AWS.Lib.Parser (element, getT, getT_)

describeNetworkInterfaceDescription
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface.
    -> EC2 m Text
describeNetworkInterfaceDescription = describeNetworkInterfaceAttribute "description" $
  element "description" $ getT "value"

describeNetworkInterfaceGroupSet
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface.
    -> EC2 m [Group]
describeNetworkInterfaceGroupSet = describeNetworkInterfaceAttribute "groupSet" groupSetSink

describeNetworkInterfaceSourceDestCheck
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface.
    -> EC2 m Bool
describeNetworkInterfaceSourceDestCheck = describeNetworkInterfaceAttribute "sourceDestCheck" $
  element "sourceDestCheck" $ getT "value"

describeNetworkInterfaceAttachment
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface.
    -> EC2 m (Maybe NetworkInterfaceAttachment)
describeNetworkInterfaceAttachment = describeNetworkInterfaceAttribute "attachment" networkInterfaceAttachmentSink

describeNetworkInterfaceAttribute
    :: (MonadBaseControl IO m, MonadResource m)
    => Text
    -> GLSink Event m a
    -> Text
    -> EC2 m a
describeNetworkInterfaceAttribute action sink networkInterface =
    ec2Query "DescribeNetworkInterfaceAttribute" params $
        getT_ "networkInterfaceId" >> sink
  where
    params =
        [ "NetworkInterfaceId" |= networkInterface
        , "Attribute" |= action
        ]

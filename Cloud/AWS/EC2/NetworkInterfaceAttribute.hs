{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Cloud.AWS.EC2.NetworkInterfaceAttribute
   ( describeNetworkInterfaceDescription
   , describeNetworkInterfaceGroupSet
   , describeNetworkInterfaceSourceDestCheck
   , describeNetworkInterfaceAttachment
   , modifyNetworkInterfaceDescription
   , modifyNetworkInterfaceSecurityGroup
   , modifyNetworkInterfaceSourceDestCheck
   , modifyNetworkInterfaceAttachment
   , resetNetworkInterfaceSourceDestCheck
   ) where

import Data.Text (Text)
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, element, (.<))

import Cloud.AWS.EC2.Internal (EC2, groupSetConv, networkInterfaceAttachmentConv)
import Cloud.AWS.EC2.Query
import Cloud.AWS.EC2.Types (Group, NetworkInterfaceAttachment(..))

describeNetworkInterfaceDescription
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface.
    -> EC2 m (Maybe Text)
describeNetworkInterfaceDescription =
    describeNetworkInterfaceAttribute "description" $
        element "description" (.< "value")

describeNetworkInterfaceGroupSet
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface.
    -> EC2 m [Group]
describeNetworkInterfaceGroupSet =
    describeNetworkInterfaceAttribute "groupSet" groupSetConv

describeNetworkInterfaceSourceDestCheck
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface.
    -> EC2 m Bool
describeNetworkInterfaceSourceDestCheck =
    describeNetworkInterfaceAttribute "sourceDestCheck" $
        element "sourceDestCheck" (.< "value")

describeNetworkInterfaceAttachment
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface.
    -> EC2 m (Maybe NetworkInterfaceAttachment)
describeNetworkInterfaceAttachment =
    describeNetworkInterfaceAttribute "attachment" networkInterfaceAttachmentConv

describeNetworkInterfaceAttribute
    :: (MonadBaseControl IO m, MonadResource m)
    => Text
    -> (XmlElement -> m a)
    -> Text
    -> EC2 m a
describeNetworkInterfaceAttribute action conv networkInterface =
    ec2Query "DescribeNetworkInterfaceAttribute" params conv
  where
    params =
        [ "NetworkInterfaceId" |= networkInterface
        , "Attribute" |= action
        ]

modifyNetworkInterfaceDescription
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface.
    -> Text -- ^ The description of the network interface.
    -> EC2 m Bool
modifyNetworkInterfaceDescription iface desc =
    modifyNetworkInterfaceAttribute iface ["Description.Value" |= desc]

modifyNetworkInterfaceSecurityGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface.
    -> [Text] -- ^ The security group ids that a network interface is in.
    -> EC2 m Bool
modifyNetworkInterfaceSecurityGroup iface groups =
    modifyNetworkInterfaceAttribute iface ["SecurityGroupId" |.#= groups]

modifyNetworkInterfaceSourceDestCheck
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface.
    -> Bool -- ^ Enables a Network Address Translation (NAT) instance in a VPC to perform NAT.
    -> EC2 m Bool
modifyNetworkInterfaceSourceDestCheck iface check =
    modifyNetworkInterfaceAttribute iface ["SourceDestCheck.Value" |= check]

modifyNetworkInterfaceAttachment
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface.
    -> Text -- ^ The ID of the interface attachment.
    -> Bool -- ^ Specifies whether to delete the attachment when terminating the instance.
    -> EC2 m Bool
modifyNetworkInterfaceAttachment iface attachment deleteOnTermination =
    modifyNetworkInterfaceAttribute iface params
  where
    params =
        [ "Attachment.AttachmentId" |= attachment
        , "Attachment.DeleteOnTermination" |= deleteOnTermination
        ]

modifyNetworkInterfaceAttribute
    :: (MonadBaseControl IO m, MonadResource m)
    => Text
    -> [QueryParam]
    -> EC2 m Bool
modifyNetworkInterfaceAttribute iface params =
    ec2Query "ModifyNetworkInterfaceAttribute" params' (.< "return")
  where
    params' = ("NetworkInterfaceId" |= iface) : params

resetNetworkInterfaceSourceDestCheck
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the network interface.
    -> EC2 m Bool
resetNetworkInterfaceSourceDestCheck =
    resetNetworkInterfaceAttribute "sourceDestCheck"

resetNetworkInterfaceAttribute
    :: (MonadBaseControl IO m, MonadResource m)
    => Text
    -> Text
    -> EC2 m Bool
resetNetworkInterfaceAttribute attrName iface =
    ec2Query "ResetNetworkInterfaceAttribute" params (.< "return")
  where
    params =
        [ "NetworkInterfaceId" |= iface
        , "Attribute" |= attrName
        ]

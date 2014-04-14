{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Cloud.AWS.EC2.Volume
    ( describeVolumes
    , createVolume
    , deleteVolume
    , attachVolume
    , detachVolume
    , describeVolumeStatus
    , enableVolumeIO
    , describeVolumeAttribute
    , modifyVolumeAttribute
    ) where

import Data.Text (Text)

import Data.Conduit
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, element, (.<))

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Params
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query

describeVolumes
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ VolumeIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m Volume)
describeVolumes vids filters =
    ec2QuerySource "DescribeVolumes" params path $
        itemConduit volumeConv
  where
    path = itemsPath "volumeSet"
    params =
        [ "VolumeId" |.#= vids
        , filtersParam filters
        ]

volumeConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m Volume
volumeConv xml = Volume
    <$> xml .< "volumeId"
    <*> xml .< "size"
    <*> xml .< "snapshotId"
    <*> xml .< "availabilityZone"
    <*> xml .< "status"
    <*> xml .< "createTime"
    <*> itemsSet "attachmentSet" attachmentConv xml
    <*> resourceTagConv xml
    <*> volumeTypeConv xml

attachmentConv :: (MonadThrow m, Applicative m) => XmlElement -> m AttachmentSetItemResponse
attachmentConv xml = AttachmentSetItemResponse
    <$> xml .< "volumeId"
    <*> xml .< "instanceId"
    <*> xml .< "device"
    <*> xml .< "status"
    <*> xml .< "attachTime"
    <*> xml .< "deleteOnTermination"

createVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => CreateVolumeRequest
    -> EC2 m Volume
createVolume param =
    ec2Query "CreateVolume" param' volumeConv
  where
    param' = createVolumeParams param

createVolumeParams :: CreateVolumeRequest -> [QueryParam]
createVolumeParams (CreateNewVolume size zone vtype) =
    [ "Size" |= size
    , "AvailabilityZone" |= zone
    ] ++ maybe [] volumeTypeParams vtype
createVolumeParams (CreateFromSnapshot sid zone size vtype) =
    [ "SnapshotId" |= sid
    , "AvailabilityZone" |= zone
    , "Size" |=? size
    ] ++ maybe [] volumeTypeParams vtype

deleteVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> EC2 m Bool
deleteVolume = ec2Delete "DeleteVolume" "VolumeId"

attachVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> Text -- ^ InstanceId
    -> Text -- ^ Device
    -> EC2 m AttachmentSetItemResponse
attachVolume volid iid dev =
    ec2Query "AttachVolume" params attachmentConv
  where
    params =
        [ "VolumeId" |= volid
        , "InstanceId" |= iid
        , "Device" |= dev
        ]

detachVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> Maybe Text -- ^ InstanceId
    -> Maybe Text -- ^ Device
    -> Maybe Bool -- ^ Force
    -> EC2 m AttachmentSetItemResponse
detachVolume volid iid dev force =
    ec2Query "DetachVolume" params attachmentConv
  where
    params =
        [ "VolumeId" |= volid
        , "InstanceId" |=? iid
        , "Device" |=? dev
        , "Force" |=? force
        ]

describeVolumeStatus
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ VolumeId
    -> [Filter] -- ^ Filters
    -> Maybe Text -- ^ next token
    -> EC2 m (ResumableSource m VolumeStatus)
describeVolumeStatus vids filters token =
    ec2QuerySource' "DescribeVolumeStatus" params token path $
       itemConduit volumeStatusConv
  where
    path = itemsPath "volumeStatusSet"
    params =
        [ "VolumeId" |.#= vids
        , filtersParam filters
        ]

volumeStatusConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m VolumeStatus
volumeStatusConv xml = VolumeStatus
    <$> xml .< "volumeId"
    <*> xml .< "availabilityZone"
    <*> element "volumeStatus" statusConv xml
    <*> itemsSet "eventsSet" eventConv xml
    <*> itemsSet "actionsSet" actionConv xml
  where
    statusConv e = VolumeStatusInfo
        <$> e .< "status"
        <*> itemsSet "details" detailConv e
    detailConv e = VolumeStatusDetail
        <$> e .< "name"
        <*> e .< "status"
    eventConv e = VolumeStatusEvent
        <$> e .< "eventType"
        <*> e .< "eventId"
        <*> e .< "description"
        <*> e .< "notBefore"
        <*> e .< "notAfter"
    actionConv e = VolumeStatusAction
        <$> e .< "code"
        <*> e .< "eventType"
        <*> e .< "eventId"
        <*> e .< "description"

modifyVolumeAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> Bool -- ^ AutoEnableIO
    -> EC2 m Bool
modifyVolumeAttribute vid enable =
    ec2Query "ModifyVolumeAttribute" params (.< "return")
  where
    params =
        [ "VolumeId" |= vid
        , "AutoEnableIO" |.+ "Value" |= enable
        ]

enableVolumeIO
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> EC2 m Bool
enableVolumeIO vid =
    ec2Query "EnableVolumeIO" params (.< "return")
  where
    params = ["VolumeId" |= vid]

-- | return (volumeId, Attribute)
describeVolumeAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> VolumeAttributeRequest
    -> EC2 m (Text, VolumeAttribute)
describeVolumeAttribute vid attr =
    ec2Query "DescribeVolumeAttribute" params $ \xml ->
        (,)
        <$> xml .< "volumeId"
        <*> volumeAttributeConv attr xml
  where
    params =
        [ "VolumeId" |= vid
        , "Attribute" |= attr
        ]

volumeAttributeConv
    :: (MonadThrow m, Applicative m)
    => VolumeAttributeRequest
    -> XmlElement
    -> m VolumeAttribute
volumeAttributeConv VolumeAttributeRequestAutoEnableIO xml
    = VolumeAttributeAutoEnableIO
    <$> element "autoEnableIO" (.< "value") xml
volumeAttributeConv VolumeAttributeRequestProductCodes xml
    = VolumeAttributeProductCodes
    <$> productCodeConv xml

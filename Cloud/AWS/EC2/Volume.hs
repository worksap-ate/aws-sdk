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

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Params
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query
import Cloud.AWS.Lib.Parser.Unordered

describeVolumes
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ VolumeIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m Volume)
describeVolumes vids filters =
    ec2QuerySource "DescribeVolumes" params $
        itemConduit "volumeSet" volumeConv
  where
    params =
        [ "VolumeId" |.#= vids
        , filtersParam filters
        ]

volumeConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m Volume
volumeConv xml = Volume
    <$> xml .< "volumeId"
    <*> xml .< "size"
    <*> xml .< "snapshotId"
    <*> xml .< "availabilityZone"
    <*> xml .< "status"
    <*> xml .< "createTime"
    <*> itemsSet xml "attachmentSet" attachmentConv
    <*> resourceTagConv xml
    <*> volumeTypeConv xml

attachmentConv :: (MonadThrow m, Applicative m) => SimpleXML -> m AttachmentSetItemResponse
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
    ec2Query "CreateVolume" param' $ xmlParser volumeConv
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
    ec2Query "AttachVolume" params $ xmlParser attachmentConv
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
    ec2Query "DetachVolume" params $ xmlParser attachmentConv
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
    ec2QuerySource' "DescribeVolumeStatus" params token $
       itemConduit "volumeStatusSet" volumeStatusConv
  where
    params =
        [ "VolumeId" |.#= vids
        , filtersParam filters
        ]

volumeStatusConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m VolumeStatus
volumeStatusConv xml = VolumeStatus
    <$> xml .< "volumeId"
    <*> xml .< "availabilityZone"
    <*> getElement xml "volumeStatus" (\xml' ->
        VolumeStatusInfo
        <$> xml' .< "status"
        <*> itemsSet xml' "details" (\xml'' ->
            VolumeStatusDetail
            <$> xml'' .< "name"
            <*> xml'' .< "status"
            )
        )
    <*> itemsSet xml "eventsSet" (\xml' ->
        VolumeStatusEvent
        <$> xml' .< "eventType"
        <*> xml' .< "eventId"
        <*> xml' .< "description"
        <*> xml' .< "notBefore"
        <*> xml' .< "notAfter"
        )
    <*> itemsSet xml "actionsSet" (\xml' ->
        VolumeStatusAction
        <$> xml' .< "code"
        <*> xml' .< "eventType"
        <*> xml' .< "eventId"
        <*> xml' .< "description"
        )

modifyVolumeAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> Bool -- ^ AutoEnableIO
    -> EC2 m Bool
modifyVolumeAttribute vid enable =
    ec2Query "ModifyVolumeAttribute" params $ xmlParser (.< "return")
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
    ec2Query "EnableVolumeIO" params $ xmlParser (.< "return")
  where
    params = ["VolumeId" |= vid]

-- | return (volumeId, Attribute)
describeVolumeAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> VolumeAttributeRequest
    -> EC2 m (Text, VolumeAttribute)
describeVolumeAttribute vid attr =
    ec2Query "DescribeVolumeAttribute" params $ xmlParser $ \xml ->
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
    -> SimpleXML
    -> m VolumeAttribute
volumeAttributeConv VolumeAttributeRequestAutoEnableIO xml
    = VolumeAttributeAutoEnableIO
    <$> getElement xml "autoEnableIO" (.< "value")
volumeAttributeConv VolumeAttributeRequestProductCodes xml
    = VolumeAttributeProductCodes
    <$> productCodeConv xml

{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}
module AWS.EC2.Volume
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

import Data.XML.Types (Event)
import Data.Conduit
import Control.Applicative
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadBaseControl, MonadResource)
#endif

import AWS.EC2.Internal
import AWS.EC2.Params
import AWS.EC2.Types
import AWS.EC2.Query
import AWS.Lib.Parser
import AWS.Util

describeVolumes
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ VolumeIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m Volume)
describeVolumes vids filters =
    ec2QuerySource "DescribeVolumes" params $
        itemConduit "volumeSet" volumeSink
  where
    params =
        [ "VolumeId" |.#= vids
        , filtersParam filters
        ]

volumeSink :: MonadThrow m
    => Consumer Event m Volume
volumeSink = Volume
    <$> getT "volumeId"
    <*> getT "size"
    <*> getT "snapshotId"
    <*> getT "availabilityZone"
    <*> getT "status"
    <*> getT "createTime"
    <*> itemsSet "attachmentSet" attachmentSink
    <*> resourceTagSink
    <*> volumeTypeSink

attachmentSink :: MonadThrow m => Consumer Event m AttachmentSetItemResponse
attachmentSink = AttachmentSetItemResponse
    <$> getT "volumeId"
    <*> getT "instanceId"
    <*> getT "device"
    <*> getT "status"
    <*> getT "attachTime"
    <*> getT "deleteOnTermination"

createVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => CreateVolumeRequest
    -> EC2 m Volume
createVolume param =
    ec2Query "CreateVolume" param' volumeSink
  where
    param' = createVolumeParams param

createVolumeParams :: CreateVolumeRequest -> [QueryParam]
createVolumeParams (CreateNewVolume size zone vtype) =
    [ "Size" |= toText size
    , "AvailabilityZone" |= zone
    ] ++ maybe [] volumeTypeParams vtype
createVolumeParams (CreateFromSnapshot sid zone size vtype) =
    [ "SnapshotId" |= sid
    , "AvailabilityZone" |= zone
    , "Size" |=? toText <$> size
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
    ec2Query "AttachVolume" params attachmentSink
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
    ec2Query "DetachVolume" params attachmentSink
  where
    params =
        [ "VolumeId" |= volid
        , "InstanceId" |=? iid
        , "Device" |=? dev
        , "Force" |=? toText <$> force
        ]

describeVolumeStatus
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ VolumeId
    -> [Filter] -- ^ Filters
    -> Maybe Text -- ^ next token
    -> EC2 m (ResumableSource m VolumeStatus)
describeVolumeStatus vids filters token =
    ec2QuerySource' "DescribeVolumeStatus" params token $
       itemConduit "volumeStatusSet" $ volumeStatusSink
  where
    params =
        [ "VolumeId" |.#= vids
        , filtersParam filters
        ]

volumeStatusSink :: MonadThrow m
    => Consumer Event m VolumeStatus
volumeStatusSink = VolumeStatus
    <$> getT "volumeId"
    <*> getT "availabilityZone"
    <*> element "volumeStatus" (VolumeStatusInfo
        <$> getT "status"
        <*> itemsSet "details" (VolumeStatusDetail
            <$> getT "name"
            <*> getT "status"
            )
        )
    <*> itemsSet "eventsSet" (VolumeStatusEvent
        <$> getT "eventType"
        <*> getT "eventId"
        <*> getT "description"
        <*> getT "notBefore"
        <*> getT "notAfter"
        )
    <*> itemsSet "actionsSet" (VolumeStatusAction
        <$> getT "code"
        <*> getT "eventType"
        <*> getT "eventId"
        <*> getT "description"
        )

modifyVolumeAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> Bool -- ^ AutoEnableIO
    -> EC2 m Bool
modifyVolumeAttribute vid enable =
    ec2Query "ModifyVolumeAttribute" params $ getT "return"
  where
    params =
        [ "VolumeId" |= vid
        , "AutoEnableIO" |.+ "Value" |= boolToText enable
        ]

enableVolumeIO
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> EC2 m Bool
enableVolumeIO vid =
    ec2Query "EnableVolumeIO" params $ getT "return"
  where
    params = ["VolumeId" |= vid]

-- | return (volumeId, Attribute)
describeVolumeAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> VolumeAttributeRequest
    -> EC2 m (Text, VolumeAttribute)
describeVolumeAttribute vid attr =
    ec2Query "DescribeVolumeAttribute" params $ (,)
        <$> getT "volumeId"
        <*> volumeAttributeSink attr
  where
    params =
        [ "VolumeId" |= vid
        , "Attribute" |= s attr
        ]
    s VolumeAttributeRequestAutoEnableIO = "autoEnableIO"
    s VolumeAttributeRequestProductCodes = "productCodes"

volumeAttributeSink
    :: MonadThrow m
    => VolumeAttributeRequest
    -> Consumer Event m VolumeAttribute
volumeAttributeSink VolumeAttributeRequestAutoEnableIO
    = VolumeAttributeAutoEnableIO
    <$> element "autoEnableIO" (getT "value")
volumeAttributeSink VolumeAttributeRequestProductCodes
    = VolumeAttributeProductCodes
    <$> productCodeSink

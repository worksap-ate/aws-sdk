{-# LANGUAGE FlexibleContexts #-}
module AWS.EC2.Volume
    ( describeVolumes
    , createVolume
    , deleteVolume
    , attachVolume
    , detachVolume
    , describeVolumeStatus
    , modifyVolumeAttribute
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

describeVolumes
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ VolumeIds
    -> [Filter] -- ^ Filters
    -> EC2 m (Source m Volume)
describeVolumes vids filters =
    ec2QuerySource "DescribeVolumes" params $
        itemConduit "volumeSet" volumeSink
  where
    params =
        [ ArrayParams "VolumeId" vids
        , FilterParams filters
        ]

volumeSink :: MonadThrow m
    => GLSink Event m Volume
volumeSink = Volume
    <$> getT "volumeId"
    <*> getF "size" textToInt
    <*> getMT "snapshotId"
    <*> getT "availabilityZone"
    <*> getF "status" volumeStatus
    <*> getF "createTime" textToTime
    <*> itemsSet "attachmentSet" attachmentSink
    <*> resourceTagSink
    <*> volumeTypeSink

attachmentSink :: MonadThrow m => GLSink Event m Attachment
attachmentSink = Attachment
    <$> getT "volumeId"
    <*> getT "instanceId"
    <*> getT "device"
    <*> getF "status" attachmentStatus
    <*> getF "attachTime" textToTime
    <*> getM "deleteOnTermination" (textToBool <$>)

volumeTypeParam :: VolumeType -> [QueryParam]
volumeTypeParam Standard = [ValueParam "VolumeType" "standard"]
volumeTypeParam (IO1 iops) =
    [ ValueParam "VolumeType" "io1"
    , ValueParam "Iops" $ toText iops
    ]

createVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => CreateVolumeRequest
    -> EC2 m Volume
createVolume param =
    ec2Query "CreateVolume" param' volumeSink
  where
    param' = createVolumeParam param

createVolumeParam :: CreateVolumeRequest -> [QueryParam]
createVolumeParam (CreateNewVolume size zone vtype) =
    [ ValueParam "Size" $ toText size
    , ValueParam "AvailabilityZone" zone
    ] ++ maybe [] volumeTypeParam vtype
createVolumeParam (CreateFromSnapshot sid zone size vtype) =
    [ ValueParam "SnapshotId" sid
    , ValueParam "AvailabilityZone" zone
    ]
    ++ maybe [] (\a -> [ValueParam "Size" $ toText a]) size
    ++ maybe [] volumeTypeParam vtype

deleteVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> EC2 m Bool
deleteVolume volid =
    ec2Query "DeleteVolume" [ValueParam "VolumeId" volid]
        $ getF "return" textToBool

attachVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> Text -- ^ InstanceId
    -> Text -- ^ Device
    -> EC2 m Attachment
attachVolume volid iid dev =
    ec2Query "AttachVolume" params attachmentSink
  where
    params =
        [ ValueParam "VolumeId" volid
        , ValueParam "InstanceId" iid
        , ValueParam "Device" dev
        ]

detachVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> Maybe Text -- ^ InstanceId
    -> Maybe Text -- ^ Device
    -> Maybe Bool -- ^ Force
    -> EC2 m Attachment
detachVolume volid iid dev force =
    ec2Query "DetachVolume" params attachmentSink
  where
    params = [ValueParam "VolumeId" volid]
        ++ maybeParams
            [ ("InstanceId", iid)
            , ("Device", dev)
            , ("Force", toText <$> force)
            ]

describeVolumeStatus
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ VolumeId
    -> [Filter] -- ^ Filters
    -> Maybe Text -- ^ next token
    -> EC2 m (Source m VolumeStatus)
describeVolumeStatus vids filters token =
    ec2QuerySource' "DescribeVolumeStatus" params token $
       itemConduit "volumeStatusSet" $ volumeStatusSink
  where
    params =
        [ ArrayParams "VolumeId" vids
        , FilterParams filters
        ]

volumeStatusSink :: MonadThrow m
    => GLSink Event m VolumeStatus
volumeStatusSink = VolumeStatus
    <$> getT "volumeId"
    <*> getT "availabilityZone"
    <*> element "volumeStatus" (VolumeStatusInfo
        <$> getF "status" volumeStatusInfoStatus
        <*> itemsSet "details" (VolumeStatusDetail
            <$> getT "name"
            <*> getT "status"
            )
        )
    <*> itemsSet "eventsSet" (VolumeStatusEvent
        <$> getT "eventType"
        <*> getT "eventId"
        <*> getT "description"
        <*> getM "notBefore" (textToTime <$>)
        <*> getM "notAfter" (textToTime <$>)
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
    ec2Query "ModifyVolumeAttribute" params returnBool
  where
    params =
        [ ValueParam "VolumeId" vid
        , ValueParam "AutoEnableIO.Value" $ boolToText enable
        ]

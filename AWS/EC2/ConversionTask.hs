{-# LANGUAGE FlexibleContexts, RecordWildCards, CPP #-}
module AWS.EC2.ConversionTask
    ( describeConversionTasks
    , cancelConversionTask
    , importVolume
    , importInstance
    ) where

import Control.Applicative ((<$>), (<*>))
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadBaseControl, MonadResource)
#endif
import Data.Conduit
import Data.Text (Text)
import Data.XML.Types (Event)

import AWS.EC2.Internal
import AWS.EC2.Query
import AWS.EC2.Types
import AWS.Lib.Parser
import AWS.Util (toText)

describeConversionTasks
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ ConversionTaskIds
    -> EC2 m (ResumableSource m ConversionTask)
describeConversionTasks ctids =
    ec2QuerySource "DescribeConversionTasks" params $
        itemConduit "conversionTasks" conversionTaskSink
  where
    params =
        [ "ConversionTaskId" |.#= ctids
        ]

conversionTaskSink
    :: MonadThrow m
    => Consumer Event m ConversionTask
conversionTaskSink = ConversionTask
    <$> getT "conversionTaskId"
    <*> getT "expirationTime"
    <*> elementM "importVolume" (
        ImportVolumeTaskDetails
        <$> getT "bytesConverted"
        <*> getT "availabilityZone"
        <*> getT "description"
        <*> element "image" diskImageDescriptionSink
        <*> element "volume" diskImageVolumeDescriptionSink
        )
    <*> elementM "importInstance" (
        ImportInstanceTaskDetails
        <$> itemsSet "volumes" (
            ImportInstanceTaskDetailItem
            <$> getT "bytesConverted"
            <*> getT "availabilityZone"
            <*> element "image" diskImageDescriptionSink
            <*> getT "description"
            <*> element "volume" diskImageVolumeDescriptionSink
            <*> getT "status"
            <*> getT "statusMessage"
            )
        <*> getT "instanceId"
        <*> getT "platform"
        <*> getT "description"
        )
    <*> getT "state"
    <*> getT "statusMessage"

diskImageDescriptionSink
    :: MonadThrow m
    => Consumer Event m DiskImageDescription
diskImageDescriptionSink = DiskImageDescription
    <$> getT "format"
    <*> getT "size"
    <*> getT "importManifestUrl"
    <*> getT "checksum"

diskImageVolumeDescriptionSink
    :: MonadThrow m
    => Consumer Event m DiskImageVolumeDescription
diskImageVolumeDescriptionSink = DiskImageVolumeDescription
    <$> getT "size"
    <*> getT "id"

cancelConversionTask
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ ConversionTaskId
    -> EC2 m Bool
cancelConversionTask =
    ec2Delete "CancelConversionTask" "ConversionTaskId"

importVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ AvailabilityZone
    -> ImportVolumeRequestImage -- ^ Image
    -> Maybe Text -- ^ Description
    -> Int -- ^ Volume Size
    -> EC2 m ConversionTask
importVolume zone image desc size =
    ec2Query "ImportVolume" params $
        element "conversionTask" conversionTaskSink
  where
    params =
        [ "AvailabilityZone" |= zone
        , "Image" |. imageParams image
        , "Description" |=? desc
        , "Volume" |.+ "Size" |= toText size
        ]
    imageParams img =
        [ "Format" |= importVolumeRequestImageFormat img
        , "Bytes" |= toText (importVolumeRequestImageBytes img)
        , "ImportManifestUrl" |= importVolumeRequestImageImportManifestUrl img
        ]

importInstance
    :: (MonadResource m, MonadBaseControl IO m)
    => Maybe Text -- ^ Description
    -> LaunchSpecification -- ^ LaunchSpecification
    -> [DiskImage] -- ^ DiskImages
    -> Platform -- ^ Platform
    -> EC2 m ConversionTask
importInstance desc ls images platform =
    ec2Query "ImportInstance" params $
        element "conversionTask" conversionTaskSink
  where
    params =
        [ "Description" |=? desc
        , "LaunchSpecification" |. launchSpecificationParams ls
        , "DiskImage" |.#. diskImageParams <$> images
        , "Platform" |= platformText platform
        ]
    launchSpecificationParams (LaunchSpecification{..}) =
        [ "Architecture" |=
            architectureText launchSpecificationArchitecture
        , "GroupName" |.#= launchSpecificationGroupNames
        , "UserData" |=? launchSpecificationUserData
        , "InstanceType" |= launchSpecificationInstanceType
        , "Placement" |.+ "AvailabilityZone" |=?
            launchSpecificationPlacementAvailabilityZone
        , "Monitoring" |.+ "Enabled" |=?
            toText <$> launchSpecificationMonitoringEnabled
        , "SubnetId" |=? launchSpecificationSubnetId
        , "InstanceInitiatedShutdownBehavior" |=?
            shutdownBehaviorText <$> launchSpecificationInstanceInitiatedShutdownBehavior
        , "PrivateIpAddress" |=?
            toText <$> launchSpecificationPrivateIpAddress
        ]
    diskImageParams (DiskImage{..}) =
        [ "Image" |.
            [ "Format" |= diskImageFormat
            , "Bytes" |= toText diskImageBytes
            , "ImportManifestUrl" |= diskImageImportManifestUrl
            , "Description" |=? diskImageDescripsion
            ]
        , "Volume" |.+ "Size" |= toText diskImageVolumeSize
        ]
    shutdownBehaviorText ShutdownBehaviorStop = "stop"
    shutdownBehaviorText ShutdownBehaviorTerminate = "terminate"
    platformText PlatformWindows = "Windows"
    platformText PlatformOther = error "Valid value is `Windows'"
    architectureText I386 = "i386"
    architectureText X86_64 = "x86_64"

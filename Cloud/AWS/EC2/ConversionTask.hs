{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Cloud.AWS.EC2.ConversionTask
    ( describeConversionTasks
    , cancelConversionTask
    , importVolume
    , importInstance
    ) where

import Control.Applicative ((<$>), (<*>), Applicative)
import Data.Conduit
import Data.Text (Text)

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Query
import Cloud.AWS.EC2.Types
import Cloud.AWS.Lib.Parser.Unordered

describeConversionTasks
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ ConversionTaskIds
    -> EC2 m (ResumableSource m ConversionTask)
describeConversionTasks ctids =
    ec2QuerySource "DescribeConversionTasks" params $
        xmlParserConduit "conversionTasks" $ \xml ->
            getElement xml "item" conversionTaskConv
  where
    params =
        [ "ConversionTaskId" |.#= ctids
        ]

conversionTaskConv
    :: (MonadThrow m, Applicative m)
    => SimpleXML -> m ConversionTask
conversionTaskConv xml = ConversionTask
    <$> xml .< "conversionTaskId"
    <*> xml .< "expirationTime"
    <*> getElementM xml "importVolume" (\xml' ->
        ImportVolumeTaskDetails
        <$> xml' .< "bytesConverted"
        <*> xml' .< "availabilityZone"
        <*> xml' .< "description"
        <*> getElement xml' "image" diskImageDescriptionConv
        <*> getElement xml' "volume" diskImageVolumeDescriptionConv
        )
    <*> getElementM xml "importInstance" (\xml' ->
        ImportInstanceTaskDetails
        <$> getElements xml' "volumes" "item" (\xml'' ->
            ImportInstanceTaskDetailItem
            <$> xml'' .< "bytesConverted"
            <*> xml'' .< "availabilityZone"
            <*> getElement xml'' "image" diskImageDescriptionConv
            <*> xml'' .< "description"
            <*> getElement xml'' "volume" diskImageVolumeDescriptionConv
            <*> xml'' .< "status"
            <*> xml'' .< "statusMessage"
            )
        <*> xml' .< "instanceId"
        <*> xml' .< "platform"
        <*> xml' .< "description"
        )
    <*> xml .< "state"
    <*> xml .< "statusMessage"

diskImageDescriptionConv
    :: (MonadThrow m, Applicative m)
    => SimpleXML -> m DiskImageDescription
diskImageDescriptionConv xml = DiskImageDescription
    <$> xml .< "format"
    <*> xml .< "size"
    <*> xml .< "importManifestUrl"
    <*> xml .< "checksum"

diskImageVolumeDescriptionConv
    :: (MonadThrow m, Applicative m)
    => SimpleXML -> m DiskImageVolumeDescription
diskImageVolumeDescriptionConv xml = DiskImageVolumeDescription
    <$> xml .< "size"
    <*> xml .< "id"

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
    ec2Query "ImportVolume" params $ xmlParser $ \xml ->
        getElement xml "conversionTask" conversionTaskConv
  where
    params =
        [ "AvailabilityZone" |= zone
        , "Image" |. imageParams image
        , "Description" |=? desc
        , "Volume" |.+ "Size" |= size
        ]
    imageParams img =
        [ "Format" |= importVolumeRequestImageFormat img
        , "Bytes" |= importVolumeRequestImageBytes img
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
    ec2Query "ImportInstance" params $ xmlParser $ \xml ->
        getElement xml "conversionTask" conversionTaskConv
  where
    params =
        [ "Description" |=? desc
        , "LaunchSpecification" |. launchSpecificationParams ls
        , "DiskImage" |.#. diskImageParams <$> images
        , "Platform" |= platform
        ]
    launchSpecificationParams (LaunchSpecification{..}) =
        [ "Architecture" |=
            launchSpecificationArchitecture
        , "GroupName" |.#= launchSpecificationGroupNames
        , "UserData" |=? launchSpecificationUserData
        , "InstanceType" |= launchSpecificationInstanceType
        , "Placement" |.+ "AvailabilityZone" |=?
            launchSpecificationPlacementAvailabilityZone
        , "Monitoring" |.+ "Enabled" |=?
            launchSpecificationMonitoringEnabled
        , "SubnetId" |=? launchSpecificationSubnetId
        , "InstanceInitiatedShutdownBehavior" |=?
            launchSpecificationInstanceInitiatedShutdownBehavior
        , "PrivateIpAddress" |=?
            launchSpecificationPrivateIpAddress
        ]
    diskImageParams (DiskImage{..}) =
        [ "Image" |.
            [ "Format" |= diskImageFormat
            , "Bytes" |= diskImageBytes
            , "ImportManifestUrl" |= diskImageImportManifestUrl
            , "Description" |=? diskImageDescripsion
            ]
        , "Volume" |.+ "Size" |= diskImageVolumeSize
        ]

{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Cloud.AWS.EC2.ConversionTask
    ( describeConversionTasks
    , cancelConversionTask
    , importVolume
    , importInstance
    ) where

import Control.Applicative ((<$>), (<*>), Applicative)
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
import Data.Conduit
import Data.Text (Text)

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<), element, elementM)

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Query
import Cloud.AWS.EC2.Types

describeConversionTasks
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ ConversionTaskIds
    -> EC2 m (ResumableSource m ConversionTask)
describeConversionTasks ctids =
    ec2QuerySource "DescribeConversionTasks" params path $
        itemConduit conversionTaskConv
  where
    path = itemsPath "conversionTasks"
    params =
        [ "ConversionTaskId" |.#= ctids
        ]

conversionTaskConv
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m ConversionTask
conversionTaskConv xml = ConversionTask
    <$> xml .< "conversionTaskId"
    <*> xml .< "expirationTime"
    <*> elementM "importVolume" (\xml' ->
        ImportVolumeTaskDetails
        <$> xml' .< "bytesConverted"
        <*> xml' .< "availabilityZone"
        <*> xml' .< "description"
        <*> element "image" diskImageDescriptionConv xml'
        <*> element "volume" diskImageVolumeDescriptionConv xml'
        ) xml
    <*> elementM "importInstance" (\xml' ->
        ImportInstanceTaskDetails
        <$> itemsSet "volumes" (\xml'' ->
            ImportInstanceTaskDetailItem
            <$> xml'' .< "bytesConverted"
            <*> xml'' .< "availabilityZone"
            <*> element "image" diskImageDescriptionConv xml''
            <*> xml'' .< "description"
            <*> element "volume" diskImageVolumeDescriptionConv xml''
            <*> xml'' .< "status"
            <*> xml'' .< "statusMessage"
            ) xml'
        <*> xml' .< "instanceId"
        <*> xml' .< "platform"
        <*> xml' .< "description"
        ) xml
    <*> xml .< "state"
    <*> xml .< "statusMessage"

diskImageDescriptionConv
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m DiskImageDescription
diskImageDescriptionConv xml = DiskImageDescription
    <$> xml .< "format"
    <*> xml .< "size"
    <*> xml .< "importManifestUrl"
    <*> xml .< "checksum"

diskImageVolumeDescriptionConv
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m DiskImageVolumeDescription
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
    ec2Query "ImportVolume" params $
        element "conversionTask" conversionTaskConv
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
    ec2Query "ImportInstance" params $
        element "conversionTask" conversionTaskConv
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

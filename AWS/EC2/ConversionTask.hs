{-# LANGUAGE FlexibleContexts #-}
module AWS.EC2.ConversionTask
    ( describeConversionTasks
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Conduit
import Data.Text (Text)
import Data.XML.Types (Event)

import AWS.EC2.Internal
import AWS.EC2.Query
import AWS.EC2.Types
import AWS.Lib.Parser

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
    => GLSink Event m ConversionTask
conversionTaskSink = ConversionTask
    <$> getT "conversionTaskId"
    <*> getT "expirationTime"
    <*> element "importVolume" (
        ImportVolumeTaskDetails
        <$> getT "bytesConverted"
        <*> getT "availabilityZone"
        <*> getT "description"
        <*> element "image" diskImageDescriptionSink
        <*> element "volume" diskImageVolumeDescriptionSink
        )
    <*> elementM "importInstance" (
        ImportInstanceTaskDetails
        <$> element "volumes" (
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
    => GLSink Event m DiskImageDescription
diskImageDescriptionSink = DiskImageDescription
    <$> getT "format"
    <*> getT "size"
    <*> getT "importManifestUrl"
    <*> getT "checksum"

diskImageVolumeDescriptionSink
    :: MonadThrow m
    => GLSink Event m DiskImageVolumeDescription
diskImageVolumeDescriptionSink = DiskImageVolumeDescription
    <$> getT "size"
    <*> getT "id"

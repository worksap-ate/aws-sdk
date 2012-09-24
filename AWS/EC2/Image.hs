{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Image
    ( describeImages
    ) where

import Data.Text (Text)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Types
import AWS.EC2.Class
import AWS.EC2.Query
import AWS.EC2.Parser

describeImages
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text]
    -> [Text]
    -> [Text]
    -> [Filter]
    -> EC2 m (Source m Image)
describeImages imageIds owners execby filters =
    ec2QuerySource "DescribeImages" params $
        itemConduit "imagesSet" imageItem
  where
    params =
        [ ArrayParams "ImageId" imageIds
        , ArrayParams "Owner" owners
        , ArrayParams "ExecutableBy" execby
        , FilterParams filters
        ]

imageItem :: MonadThrow m
    => GLSink Event m Image
imageItem = image
    <$> getT "imageId"
    <*> getT "imageLocation"
    <*> getF "imageState" t2imageState
    <*> getT "imageOwnerId"
    <*> getF "isPublic" t2bool
    <*> productCodeSink
    <*> getT "architecture"
    <*> getF "imageType" t2imageType
    <*> getMT "kernelId"
    <*> getMT "ramdiskId"
    <*> getM "platform" t2platform
    <*> stateReasonSink
    <*> getMT "imageOwnerAlias"
    <*> getM "name" t2emptxt
    <*> getM "description" t2emptxt
    <*> getF "rootDeviceType" t2rootDeviceType
    <*> getMT "rootDeviceName"
    <*> itemsSet "blockDeviceMapping" (
        blockDeviceMapping
        <$> getT "deviceName"
        <*> getMT "virtualName"
        <*> elementM "ebs" (
            ebsBlockDevice
            <$> getMT "snapshotId"
            <*> getF "volumeSize" t2dec
            <*> getF "deleteOnTermination" t2bool
            <*> getF "volumeType" t2volumeType
            <*> getM "iops" t2iops
            )
        )
    <*> getF "virtualizationType" t2virtualizationType
    <*> resourceTagSink
    <*> getF "hypervisor" t2hypervisor

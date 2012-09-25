{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Image
    ( describeImages
    , createImage
    ) where

import Data.Text (Text)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Types
import AWS.EC2.Params
import AWS.EC2.Class
import AWS.EC2.Query
import AWS.EC2.Parser
import AWS.Util

describeImages
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ ImageIds
    -> [Text] -- ^ Owners (User Ids)
    -> [Text] -- ^ ExecutedBy (User Ids)
    -> [Filter] -- ^ Filers
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
    <*> getF "imageState" imageState
    <*> getT "imageOwnerId"
    <*> getF "isPublic" t2bool
    <*> productCodeSink
    <*> getT "architecture"
    <*> getF "imageType" imageType
    <*> getMT "kernelId"
    <*> getMT "ramdiskId"
    <*> getM "platform" platform
    <*> stateReasonSink
    <*> getMT "imageOwnerAlias"
    <*> getM "name" t2emptxt
    <*> getM "description" t2emptxt
    <*> getF "rootDeviceType" rootDeviceType
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
            <*> (volumeType
                 <$> getT "volumeType"
                 <*> getM "iops" (t2dec <$>)
                )
            )
        )
    <*> getF "virtualizationType" virtualizationType
    <*> resourceTagSink
    <*> getF "hypervisor" hypervisor

createImage
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InstanceId
    -> Text -- ^ Name
    -> Maybe Text -- ^ Description
    -> Bool -- ^ NoReboot
    -> [BlockDeviceMappingParam] -- ^ BlockDeviceMapping
    -> EC2 m Text
createImage iid name desc noReboot bdms =
    ec2Query "CreateImage" params $
        yield =<< getT "imageId"
  where
    param n = maybe [] (\a -> [ValueParam n a])
    params =
        [ ValueParam "InstanceId" iid
        , ValueParam "Name" name
        , ValueParam "NoReboot" (boolToText noReboot)
        ] ++ param "Description" desc
          ++ [blockDeviceMappingParams bdms]

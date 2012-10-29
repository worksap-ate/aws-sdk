{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Image
    ( describeImages
    , createImage
    , registerImage
    , deregisterImage
    ) where

import Data.Text (Text)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Internal
import AWS.EC2.Types
import AWS.EC2.Params
import AWS.EC2.Query
import AWS.Lib.Parser
import AWS.Util

describeImages
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ ImageIds
    -> [Text] -- ^ Owners (User Ids)
    -> [Text] -- ^ ExecutedBy (User Ids)
    -> [Filter] -- ^ Filers
    -> EC2 m (ResumableSource m Image)
describeImages imageIds owners execby filters =
    ec2QuerySource "DescribeImages" params $ itemConduit "imagesSet" imageItem
--    ec2QueryDebug "DescribeImages" params
  where
    params =
        [ ArrayParams "ImageId" imageIds
        , ArrayParams "Owner" owners
        , ArrayParams "ExecutableBy" execby
        , FilterParams filters
        ]

imageItem :: MonadThrow m
    => GLSink Event m Image
imageItem = Image
    <$> getT "imageId"
    <*> getT "imageLocation"
    <*> getF "imageState" imageState
    <*> getT "imageOwnerId"
    <*> getF "isPublic" textToBool
    <*> productCodeSink
    <*> getT "architecture"
    <*> getF "imageType" imageType
    <*> getMT "kernelId"
    <*> getMT "ramdiskId"
    <*> getM "platform" platform
    <*> stateReasonSink
    <*> getM "viridianEnabled" (textToBool <$>)
    <*> getMT "imageOwnerAlias"
    <*> getM "name" orEmpty
    <*> getM "description" orEmpty
    <*> itemsSet "billingProducts" (getT "billingProduct")
    <*> getF "rootDeviceType" rootDeviceType
    <*> getMT "rootDeviceName"
    <*> itemsSet "blockDeviceMapping" (
        BlockDeviceMapping
        <$> getT "deviceName"
        <*> getMT "virtualName"
        <*> elementM "ebs" (
            EbsBlockDevice
            <$> getMT "snapshotId"
            <*> getF "volumeSize" textToInt
            <*> getF "deleteOnTermination" textToBool
            <*> volumeTypeSink
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
    ec2Query "CreateImage" params $ getT "imageId"
  where
    param n = maybe [] (\a -> [ValueParam n a])
    params =
        [ ValueParam "InstanceId" iid
        , ValueParam "Name" name
        , ValueParam "NoReboot" (boolToText noReboot)
        ] ++ param "Description" desc
          ++ [blockDeviceMappingParams bdms]

registerImage
    :: (MonadResource m, MonadBaseControl IO m)
    => RegisterImageRequest
    -> EC2 m Text
registerImage req =
    ec2Query "RegisterImage" params $ getT "imageId"
  where
    params = [ValueParam "Name" $ rirName req]
        ++ [blockDeviceMappingParams $ rirBlockDeviceMappings req]
        ++ maybeParams
            [ ("ImageLocation", rirImageLocation req)
            , ("Description", rirDescription req)
            , ("Architecture", rirArchitecture req)
            , ("KernelId", rirKernelId req)
            , ("RamdiskId", rirRamdiskId req)
            , ("RootDeviceName", rirRootDeviceName req)
            ] 

deregisterImage
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ ImageId
    -> EC2 m Bool
deregisterImage iid =
    ec2Query "DeregisterImage" params returnBool
  where
    params = [ValueParam "ImageId" iid]

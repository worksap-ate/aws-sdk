{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Image
    ( describeImages
    , createImage
    , registerImage
    , deregisterImage
    , describeImageAttribute
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative
import Control.Monad (join)

import AWS.EC2.Convert
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
    <*> blockDeviceMappingSink
    <*> getF "virtualizationType" virtualizationType
    <*> resourceTagSink
    <*> getF "hypervisor" hypervisor

blockDeviceMappingSink :: MonadThrow m => GLSink Event m [BlockDeviceMapping]
blockDeviceMappingSink = itemsSet "blockDeviceMapping" (
    BlockDeviceMapping
    <$> getT "deviceName"
    <*> getMT "virtualName"
    <*> elementM "ebs" (
        EbsBlockDevice
        <$> getMT "snapshotId"
        <*> getF "volumeSize" textRead
        <*> getF "deleteOnTermination" textToBool
        <*> volumeTypeSink
        )
    )

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
    params = [ValueParam "Name" $ registerImageRequestName req]
        ++ [blockDeviceMappingParams
            $ registerImageRequestBlockDeviceMappings req]
        ++ maybeParams
            [ ("ImageLocation"
              , registerImageRequestImageLocation req
              )
            , ("Description", registerImageRequestDescription req)
            , ("Architecture"
              , registerImageRequestArchitecture req
              )
            , ("KernelId", registerImageRequestKernelId req)
            , ("RamdiskId", registerImageRequestRamdiskId req)
            , ("RootDeviceName"
              , registerImageRequestRootDeviceName req
              )
            ]

deregisterImage
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ ImageId
    -> EC2 m Bool
deregisterImage iid =
    ec2Query "DeregisterImage" params returnBool
  where
    params = [ValueParam "ImageId" iid]

describeImageAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ ImageId
    -> AMIAttribute -- ^ Attribute
    -> EC2 m AMIAttributeDescription
describeImageAttribute iid attr =
    ec2Query "DescribeImageAttribute" params $ AMIAttributeDescription
        <$> getT "imageId"
        <*> itemsSet "launchPermission"
            (LaunchPermissionItem
            <$> getT "group"
            <*> getT "userId")
        <*> itemsSet "productCodes"
            (ProductCodeItem
            <$> getT "productCode")
        <*> getMMT "kernel"
        <*> getMMT "ramdisk"
        <*> getMMT "description"
        <*> blockDeviceMappingSink
  where
    getMMT name = join <$> elementM name (getMT "value")
    params = [ ValueParam "ImageId" iid
             , ValueParam "Attribute" param
             ]
    param :: Text
    param | attr == AMIDescription        = "description"
          | attr == AMIKernel             = "kernel"
          | attr == AMIRamdisk            = "ramdisk"
          | attr == AMILaunchPermission   = "launchPermission"
          | attr == AMIProductCodes       = "productCodes"
          | attr == AMIBlockDeviceMapping = "blockDeviceMapping"
          | otherwise                     = err "AMIAttribute" $ T.pack $ show attr

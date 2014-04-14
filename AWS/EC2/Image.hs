{-# LANGUAGE FlexibleContexts, CPP #-}

module AWS.EC2.Image
    ( describeImages
    , createImage
    , registerImage
    , deregisterImage
    , describeImageAttribute
    , modifyImageAttribute
    ) where

import Data.Text (Text)
import Data.XML.Types (Event)
import Data.Conduit
import Control.Applicative
import Control.Monad (join)
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadBaseControl, MonadResource)
#endif

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
  where
    params =
        [ "ImageId" |.#= imageIds
        , "Owner" |.#= owners
        , "ExecutableBy" |.#= execby
        , filtersParam filters
        ]

imageItem :: MonadThrow m
    => Consumer Event m Image
imageItem = Image
    <$> getT "imageId"
    <*> getT "imageLocation"
    <*> getT "imageState"
    <*> getT "imageOwnerId"
    <*> getT "isPublic"
    <*> productCodeSink
    <*> getT "architecture"
    <*> getT "imageType"
    <*> getT "kernelId"
    <*> getT "ramdiskId"
    <*> getT "platform"
    <*> stateReasonSink
    <*> getT "viridianEnabled"
    <*> getT "imageOwnerAlias"
    <*> getT "name"
    <*> getT "description"
    <*> itemsSet "billingProducts" (getT "billingProduct")
    <*> getT "rootDeviceType"
    <*> getT "rootDeviceName"
    <*> blockDeviceMappingSink
    <*> getT "virtualizationType"
    <*> resourceTagSink
    <*> getT "hypervisor"

blockDeviceMappingSink :: MonadThrow m => Consumer Event m [BlockDeviceMapping]
blockDeviceMappingSink = itemsSet "blockDeviceMapping" (
    BlockDeviceMapping
    <$> getT "deviceName"
    <*> getT "virtualName"
    <*> elementM "ebs" (
        EbsBlockDevice
        <$> getT "snapshotId"
        <*> getT "volumeSize"
        <*> getT "deleteOnTermination"
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
    params =
        [ "InstanceId" |= iid
        , "Name" |= name
        , "NoReboot" |= boolToText noReboot
        , "Description" |=? desc
        , blockDeviceMappingsParam bdms
        ]

registerImage
    :: (MonadResource m, MonadBaseControl IO m)
    => RegisterImageRequest
    -> EC2 m Text
registerImage req =
    ec2Query "RegisterImage" params $ getT "imageId"
  where
    params =
        [ "Name" |= registerImageRequestName req
        , "ImageLocation" |=? registerImageRequestImageLocation req
        , "Description" |=? registerImageRequestDescription req
        , "Architecture" |=? registerImageRequestArchitecture req
        , "KernelId" |=? registerImageRequestKernelId req
        , "RamdiskId" |=? registerImageRequestRamdiskId req
        , "RootDeviceName" |=? registerImageRequestRootDeviceName req
        , blockDeviceMappingsParam $
            registerImageRequestBlockDeviceMappings req
        ]

deregisterImage
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ ImageId
    -> EC2 m Bool
deregisterImage iid =
    ec2Query "DeregisterImage" params $ getT "return"
  where
    params = ["ImageId" |= iid]

describeImageAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ ImageId
    -> AMIAttribute -- ^ Attribute
    -> EC2 m AMIAttributeDescription
describeImageAttribute iid attr =
    ec2Query "DescribeImageAttribute" params $ AMIAttributeDescription
        <$> getT "imageId"
        <*> itemsSet "launchPermission" launchPermissionItemSink
        <*> itemsSet "productCodes"
            (ProductCodeItem
            <$> getT "productCode")
        <*> getMMT "kernel"
        <*> getMMT "ramdisk"
        <*> getMMT "description"
        <*> blockDeviceMappingSink
  where
    getMMT name = join <$> elementM name (getT "value")
    params = [ "ImageId" |= iid
             , "Attribute" |= attrText attr
             ]
    attrText AMIDescription        = "description"
    attrText AMIKernel             = "kernel"
    attrText AMIRamdisk            = "ramdisk"
    attrText AMILaunchPermission   = "launchPermission"
    attrText AMIProductCodes       = "productCodes"
    attrText AMIBlockDeviceMapping = "blockDeviceMapping"

launchPermissionItemSink :: MonadThrow m => Consumer Event m LaunchPermissionItem
launchPermissionItemSink = do
    mg <- elementM "group" text
    case mg of
        Just g -> return $ LaunchPermissionItemGroup g
        Nothing -> LaunchPermissionItemUserId <$> getT "userId"

modifyImageAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ ImageId
    -> Maybe LaunchPermission -- ^ LaunchPermission
    -> [Text] -- ^ ProductCode
    -> Maybe Text -- ^ Description
    -> EC2 m Bool
modifyImageAttribute iid lp pcs desc =
    ec2Query "ModifyImageAttribute" params $ getT "return"
  where
    params =
        [ "ImageId" |= iid
        , "ProductCode" |.#= pcs
        , "LaunchPermission" |.? launchPermissionParams <$> lp
        , "Description" |.+ "Value" |=? desc
        ]

launchPermissionParams :: LaunchPermission -> [QueryParam]
launchPermissionParams lp =
    [ "Add" |.#. map itemParams (launchPermissionAdd lp)
    , "Remove" |.#. map itemParams (launchPermissionRemove lp)
    ]
  where
    itemParams (LaunchPermissionItemGroup g) = ["Group" |= g]
    itemParams (LaunchPermissionItemUserId u) = ["UserId" |= u]

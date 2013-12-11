{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.EC2.Image
    ( describeImages
    , createImage
    , registerImage
    , deregisterImage
    , describeImageAttribute
    , modifyImageAttribute
    ) where

import Data.Text (Text)
import Data.Conduit
import Control.Applicative
import Control.Monad (join)

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Params
import Cloud.AWS.EC2.Query
import Cloud.AWS.Lib.Parser.Unordered

describeImages
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ ImageIds
    -> [Text] -- ^ Owners (User Ids)
    -> [Text] -- ^ ExecutedBy (User Ids)
    -> [Filter] -- ^ Filers
    -> EC2 m (ResumableSource m Image)
describeImages imageIds owners execby filters =
    ec2QuerySource "DescribeImages" params $
        itemConduit' "imagesSet" imageItem
  where
    params =
        [ "ImageId" |.#= imageIds
        , "Owner" |.#= owners
        , "ExecutableBy" |.#= execby
        , filtersParam filters
        ]

imageItem :: (MonadThrow m, Applicative m)
    => SimpleXML -> m Image
imageItem xml = Image
    <$> xml .< "imageId"
    <*> xml .< "imageLocation"
    <*> xml .< "imageState"
    <*> xml .< "imageOwnerId"
    <*> xml .< "isPublic"
    <*> productCodeConv xml
    <*> xml .< "architecture"
    <*> xml .< "imageType"
    <*> xml .< "kernelId"
    <*> xml .< "ramdiskId"
    <*> xml .< "platform"
    <*> stateReasonConv xml
    <*> xml .< "viridianEnabled"
    <*> xml .< "imageOwnerAlias"
    <*> xml .< "name"
    <*> xml .< "description"
    <*> itemsSet' xml "billingProducts" (.< "billingProduct")
    <*> xml .< "rootDeviceType"
    <*> xml .< "rootDeviceName"
    <*> blockDeviceMappingConv xml
    <*> xml .< "virtualizationType"
    <*> resourceTagConv xml
    <*> xml .< "hypervisor"

blockDeviceMappingConv :: (MonadThrow m, Applicative m) => SimpleXML -> m [BlockDeviceMapping]
blockDeviceMappingConv xml = itemsSet' xml "blockDeviceMapping" (\xml' ->
    BlockDeviceMapping
    <$> xml' .< "deviceName"
    <*> xml' .< "virtualName"
    <*> getElementM xml' "ebs" (\xml'' ->
        EbsBlockDevice
        <$> xml'' .< "snapshotId"
        <*> xml'' .< "volumeSize"
        <*> xml'' .< "deleteOnTermination"
        <*> volumeTypeConv xml''
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
    ec2Query "CreateImage" params $ xmlParser (.< "imageId")
  where
    params =
        [ "InstanceId" |= iid
        , "Name" |= name
        , "NoReboot" |= noReboot
        , "Description" |=? desc
        , blockDeviceMappingsParam bdms
        ]

registerImage
    :: (MonadResource m, MonadBaseControl IO m)
    => RegisterImageRequest
    -> EC2 m Text
registerImage req =
    ec2Query "RegisterImage" params $ xmlParser (.< "imageId")
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
    ec2Query "DeregisterImage" params $ xmlParser (.< "return")
  where
    params = ["ImageId" |= iid]

describeImageAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ ImageId
    -> AMIAttribute -- ^ Attribute
    -> EC2 m AMIAttributeDescription
describeImageAttribute iid attr =
    ec2Query "DescribeImageAttribute" params $ xmlParser $ \xml ->
        AMIAttributeDescription
        <$> xml .< "imageId"
        <*> itemsSet' xml "launchPermission" launchPermissionItemConv
        <*> itemsSet' xml "productCodes" (\xml' ->
            ProductCodeItem
            <$> xml' .< "productCode"
            )
        <*> getMMT xml "kernel"
        <*> getMMT xml "ramdisk"
        <*> getMMT xml "description"
        <*> blockDeviceMappingConv xml
  where
    getMMT xml name = join <$> getElementM xml name (.< "value")
    params = [ "ImageId" |= iid
             , "Attribute" |= attr
             ]

launchPermissionItemConv :: (MonadThrow m, Applicative m) => SimpleXML -> m LaunchPermissionItem
launchPermissionItemConv xml = do
    mg <- getElementM xml "group" content
    case mg of
        Just g -> return $ LaunchPermissionItemGroup g
        Nothing -> LaunchPermissionItemUserId <$> xml .< "userId"

modifyImageAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ ImageId
    -> Maybe LaunchPermission -- ^ LaunchPermission
    -> [Text] -- ^ ProductCode
    -> Maybe Text -- ^ Description
    -> EC2 m Bool
modifyImageAttribute iid lp pcs desc =
    ec2Query "ModifyImageAttribute" params $ xmlParser (.< "return")
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

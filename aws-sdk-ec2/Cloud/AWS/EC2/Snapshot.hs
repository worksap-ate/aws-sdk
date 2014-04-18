{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.EC2.Snapshot
    ( describeSnapshots
    , createSnapshot
    , deleteSnapshot
    , copySnapshot
    , describeSnapshotAttribute
    , modifySnapshotAttribute
    , resetSnapshotAttribute
    ) where

import Data.Text (Text)
import Data.Conduit
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<), elementM, content)

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query

describeSnapshots
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ SnapshotIds
    -> [Text] -- ^ Owners (UserId)
    -> [Text] -- ^ RestorableBy (UserId)
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m Snapshot)
describeSnapshots ssids owners restby filters =
    ec2QuerySource "DescribeSnapshots" params path $
        itemConduit snapshotConv
  where
    path = itemsPath "snapshotSet"
    params =
        [ "SnapshotId" |.#= ssids
        , "Owner" |.#= owners
        , "RestorableBy" |.#= restby
        , filtersParam filters
        ]

snapshotConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m Snapshot
snapshotConv xml = Snapshot
        <$> xml .< "snapshotId"
        <*> xml .< "volumeId"
        <*> xml .< "status"
        <*> xml .< "startTime"
        <*> xml .< "progress"
        <*> xml .< "ownerId"
        <*> xml .< "volumeSize"
        <*> xml .< "description"
        <*> xml .< "ownerAlias"
        <*> resourceTagConv xml

createSnapshot
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> Maybe Text -- ^ Description
    -> EC2 m Snapshot
createSnapshot volid desc =
    ec2Query "CreateSnapshot" params snapshotConv
  where
    params =
        [ "VolumeId" |= volid
        , "Description" |=? desc
        ]

deleteSnapshot
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SnapshotId
    -> EC2 m Bool
deleteSnapshot ssid =
    ec2Query "DeleteSnapshot" params (.< "return")
  where
    params = ["SnapshotId" |= ssid]

copySnapshot
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SourceRegion
    -> Text -- ^ SourceSnapshotId
    -> Maybe Text -- ^ Description
    -> EC2 m Text
copySnapshot region sid desc =
    ec2Query "CopySnapshot" params (.< "snapshotId")
  where
    params = [ "SourceRegion" |= region
             , "SourceSnapshotId" |= sid
             , "Description" |=? desc
             ]

describeSnapshotAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SnapshotId
    -> SnapshotAttributeRequest -- ^ Attribute
    -> EC2 m SnapshotAttribute
describeSnapshotAttribute ssid attr =
    ec2Query "DescribeSnapshotAttribute" params
        snapshotAttributeConv
  where
    params =
        [ "SnapshotId" |= ssid
        , "Attribute" |= attr
        ]

snapshotAttributeConv
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m SnapshotAttribute
snapshotAttributeConv xml = SnapshotAttribute
    <$> xml .< "snapshotId"
    <*> itemsSet "createVolumePermission" createVolumePermissionItemConv xml
    <*> productCodeConv xml

createVolumePermissionItemConv :: (MonadThrow m, Applicative m) => XmlElement -> m CreateVolumePermissionItem
createVolumePermissionItemConv xml = do
    mg <- elementM "group" content xml
    case mg of
        Just g -> return $ CreateVolumePermissionItemGroup g
        Nothing -> CreateVolumePermissionItemUserId <$> xml .< "userId"

modifySnapshotAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SnapshotId
    -> CreateVolumePermission -- ^ CreateVolumePermission
    -> EC2 m Bool
modifySnapshotAttribute ssid cvp =
    ec2Query "ModifySnapshotAttribute" params (.< "return")
  where
    params =
        [ "SnapshotId" |= ssid
        , "CreateVolumePermission" |. createVolumePermissionParams cvp
        ]

createVolumePermissionParams
    :: CreateVolumePermission
    -> [QueryParam]
createVolumePermissionParams cvp =
    [ "Add" |.#. itemParams <$> createVolumePermissionAdd cvp
    , "Remove" |.#. itemParams <$> createVolumePermissionRemove cvp
    ]
  where
    itemParams (CreateVolumePermissionItemUserId u) = ["UserId" |= u]
    itemParams (CreateVolumePermissionItemGroup g) = ["Group" |= g]

resetSnapshotAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SnapshotId
    -> ResetSnapshotAttributeRequest -- ^ Attribute
    -> EC2 m Bool
resetSnapshotAttribute ssid attr =
    ec2Query "ResetSnapshotAttribute" params (.< "return")
  where
    params =
        [ "SnapshotId" |= ssid
        , "Attribute" |= attr
        ]

{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Snapshot
    ( describeSnapshots
    , createSnapshot
    , deleteSnapshot
    , copySnapshot
    , describeSnapshotAttribute
    , modifySnapshotAttribute
    , resetSnapshotAttribute
    ) where

import Data.Text (Text)
import Data.XML.Types (Event)
import Data.Conduit
import Control.Applicative

import AWS.EC2.Internal
import AWS.EC2.Types
import AWS.EC2.Query
import AWS.Lib.Parser

describeSnapshots
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ SnapshotIds
    -> [Text] -- ^ Owners (UserId)
    -> [Text] -- ^ RestorableBy (UserId)
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m Snapshot)
describeSnapshots ssids owners restby filters =
    ec2QuerySource "DescribeSnapshots" params $
        itemConduit "snapshotSet" snapshotSink
  where
    params =
        [ "SnapshotId" |.#= ssids
        , "Owner" |.#= owners
        , "RestorableBy" |.#= restby
        , filtersParam filters
        ]

snapshotSink :: MonadThrow m
    => Consumer Event m Snapshot
snapshotSink = Snapshot
        <$> getT "snapshotId"
        <*> getT "volumeId"
        <*> getT "status"
        <*> getT "startTime"
        <*> getT "progress"
        <*> getT "ownerId"
        <*> getT "volumeSize"
        <*> getT "description"
        <*> getT "ownerAlias"
        <*> resourceTagSink

createSnapshot
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> Maybe Text -- ^ Description
    -> EC2 m Snapshot
createSnapshot volid desc =
    ec2Query "CreateSnapshot" params snapshotSink
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
    ec2Query "DeleteSnapshot" params $ getT "return"
  where
    params = ["SnapshotId" |= ssid]

copySnapshot
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SourceRegion
    -> Text -- ^ SourceSnapshotId
    -> Maybe Text -- ^ Description
    -> EC2 m Text
copySnapshot region sid desc =
    ec2Query "CopySnapshot" params $ getT "snapshotId"
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
    ec2Query "DescribeSnapshotAttribute" params snapshotAttributeSink
  where
    params =
        [ "SnapshotId" |= ssid
        , "Attribute" |= attrText attr
        ]
    attrText SnapshotAttributeRequestCreateVolumePermission
        = "createVolumePermission"
    attrText SnapshotAttributeRequestProductCodes
        = "productCodes"

snapshotAttributeSink
    :: MonadThrow m
    => Consumer Event m SnapshotAttribute
snapshotAttributeSink = SnapshotAttribute
    <$> getT "snapshotId"
    <*> itemsSet "createVolumePermission" (
        CreateVolumePermissionItem
        <$> getT "userId"
        <*> getT "group"
        )
    <*> productCodeSink

modifySnapshotAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SnapshotId
    -> CreateVolumePermission -- ^ CreateVolumePermission
    -> EC2 m Bool
modifySnapshotAttribute ssid cvp =
    ec2Query "ModifySnapshotAttribute" params $ getT "return"
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
    itemParams item =
        [ "UserId" |=? createVolumePermissionItemUserId item
        , "Group" |=? createVolumePermissionItemGroup item
        ]

resetSnapshotAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SnapshotId
    -> ResetSnapshotAttributeRequest -- ^ Attribute
    -> EC2 m Bool
resetSnapshotAttribute ssid attr =
    ec2Query "ResetSnapshotAttribute" params $ getT "return"
  where
    params =
        [ "SnapshotId" |= ssid
        , "Attribute" |= attrText attr
        ]
    attrText ResetSnapshotAttributeRequestCreateVolumePermission
        = "createVolumePermission"

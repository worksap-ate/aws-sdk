{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Snapshot
    ( describeSnapshots
    , createSnapshot
    , deleteSnapshot
    , copySnapshot
    , describeSnapshotAttribute
    , modifySnapshotAttribute
    ) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
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
        [ ArrayParams "SnapshotId" ssids
        , ArrayParams "Owner" owners
        , ArrayParams "RestorableBy" restby
        , FilterParams filters
        ]

snapshotSink :: MonadThrow m
    => GLSink Event m Snapshot
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
    params = [ValueParam "VolumeId" volid]
        ++ maybe [] (\a -> [ValueParam "Description" a]) desc

deleteSnapshot
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SnapshotId
    -> EC2 m Bool
deleteSnapshot ssid =
    ec2Query "DeleteSnapshot" params $ getT "return"
  where
    params = [ValueParam "SnapshotId" ssid]

copySnapshot
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SourceRegion
    -> Text -- ^ SourceSnapshotId
    -> Maybe Text -- ^ Description
    -> EC2 m Text
copySnapshot region sid desc =
    ec2Query "CopySnapshot" params $ getT "snapshotId"
  where
    params = [ ValueParam "SourceRegion" region
             , ValueParam "SourceSnapshotId" sid
             ] ++ maybeParams [("Description", desc)]

describeSnapshotAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SnapshotId
    -> SnapshotAttributeRequest -- ^ Attribute
    -> EC2 m SnapshotAttribute
describeSnapshotAttribute ssid attr =
    ec2Query "DescribeSnapshotAttribute" params snapshotAttributeSink
  where
    params =
        [ ValueParam "SnapshotId" ssid
        , ValueParam "Attribute" $ attrText attr
        ]
    attrText SnapshotAttributeRequestCreateVolumePermission
        = "createVolumePermission"
    attrText SnapshotAttributeRequestProductCodes
        = "productCodes"

snapshotAttributeSink
    :: MonadThrow m
    => GLSink Event m SnapshotAttribute
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
        [ ValueParam "SnapshotId" ssid
        ] ++ createVolumePermissionParams cvp

createVolumePermissionParams
    :: CreateVolumePermission
    -> [QueryParam]
createVolumePermissionParams cvp =
    [ param "Add" $ createVolumePermissionAdd cvp
    , param "Remove" $ createVolumePermissionRemove cvp
    ]
  where
    toTuples (CreateVolumePermissionItem user group) =
        filter (not . T.null . snd) .
        map (fmap (fromMaybe "")) $
        [("UserId", user), ("Group", group)]
    param t =
        StructArrayParams ("CreateVolumePermission." <> t) . map toTuples

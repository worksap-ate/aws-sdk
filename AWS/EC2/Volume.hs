{-# LANGUAGE FlexibleContexts #-}
module AWS.EC2.Volume
    ( describeVolumes
    , createVolume
    , CreateVolumeParam(..)
    , deleteVolume
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
import AWS.Util

describeVolumes
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ VolumeIds
    -> [Filter] -- ^ Filters
    -> EC2 m (Source m Volume)
describeVolumes vids filters =
    ec2QuerySource "DescribeVolumes" params $
        itemConduit "volumeSet" volumeSink
  where
    params =
        [ ArrayParams "VolumeId" vids
        , FilterParams filters
        ]

volumeSink :: MonadThrow m
    => GLSink Event m Volume
volumeSink = Volume
    <$> getT "volumeId"
    <*> getF "size" textToInt
    <*> getMT "snapshotId"
    <*> getT "availabilityZone"
    <*> getF "status" volumeStatus
    <*> getF "createTime" textToTime
    <*> itemsSet "attachmentSet" (
        Attachment
        <$> getT "volumeId"
        <*> getT "instanceId"
        <*> getT "device"
        <*> getF "status" attachmentStatus
        <*> getF "attachTime" textToTime
        <*> getF "deleteOnTermination" textToBool
        )
    <*> resourceTagSink
    <*> volumeTypeSink

volumeTypeParam :: VolumeType -> [QueryParam]
volumeTypeParam Standard = [ValueParam "VolumeType" "standard"]
volumeTypeParam (IO1 iops) =
    [ ValueParam "VolumeType" "io1"
    , ValueParam "Iops" $ toText iops
    ]

createVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => CreateVolumeParam
    -> EC2 m Volume
createVolume param =
    ec2Query "CreateVolume" param' volumeSink
  where
    param' = createVolumeParam param

data CreateVolumeParam
    = CreateNewVolume
        { cnvSize :: Int
        , cnvAvailabilityZone :: Text
        , cnvVolumeType :: Maybe VolumeType
        }
    | CreateFromSnapshot
        { cfsSnapshotId :: Text
        , cfsAvailabilityZone :: Text
        , cfsSize :: Maybe Int
        , cfsVolumeType :: Maybe VolumeType
        }
  deriving (Show)

createVolumeParam :: CreateVolumeParam -> [QueryParam]
createVolumeParam (CreateNewVolume size zone vtype) =
    [ ValueParam "Size" $ toText size
    , ValueParam "AvailabilityZone" zone
    ] ++ maybe [] volumeTypeParam vtype
createVolumeParam (CreateFromSnapshot sid zone size vtype) =
    [ ValueParam "SnapshotId" sid
    , ValueParam "AvailabilityZone" zone
    ]
    ++ maybe [] (\a -> [ValueParam "Size" $ toText a]) size
    ++ maybe [] volumeTypeParam vtype

deleteVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> EC2 m Bool
deleteVolume volid =
    ec2Query "DeleteVolume" [ValueParam "VolumeId" volid]
        $ getF "return" textToBool

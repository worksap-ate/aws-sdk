{-# LANGUAGE FlexibleContexts #-}
module AWS.EC2.Volume
    ( describeVolumes
    , createVolume
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
        attachment
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
    => Maybe Int -- ^ Size
    -> Maybe Text -- ^ SnapshotId
    -> Maybe Text -- ^ AvailabilityZone
    -> Maybe VolumeType
    -> EC2 m Volume
createVolume size sid zone vtype =
    ec2Query "CreateVolume" params volumeSink
  where
    f name = maybe [] (\a -> [ValueParam name a])
    params = maybe [] volumeTypeParam vtype
        ++ (uncurry f =<<
            [ ("Size", toText <$> size)
            , ("SnapshotId", sid)
            , ("AvailabilityZone", zone)
            ])

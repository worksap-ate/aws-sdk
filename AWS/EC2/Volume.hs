{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Volume
    ( describeVolumes
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
    ec2QuerySource "DescribeVolumes" params volumeSet
  where
    params =
        [ ArrayParams "VolumeId" vids
        , FilterParams filters
        ]
    volumeSet :: MonadThrow m
        => GLConduit Event m Volume
    volumeSet = itemConduit "volumeSet" $
        Volume
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

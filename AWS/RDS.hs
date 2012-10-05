{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.RDS
    ( -- * RDS Environment
      RDS
    , runRDS
    , setRegion
      -- * DBInstance
    , describeDBInstances
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Conduit
import Control.Applicative
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import qualified Network.HTTP.Conduit as HTTP
import Data.Monoid ((<>))
import qualified Text.XML.Stream.Parse as XML
import Data.XML.Types (Event(..))
import Data.Maybe (catMaybes)

import AWS.Class
import AWS.Util
import AWS.Lib.Query
import AWS.Lib.Parser

import AWS
import AWS.RDS.Types

--import qualified Data.Conduit.Binary as CB

apiVersion :: ByteString
apiVersion = "2012-09-17"

type RequestId = Text

type RDS m a = AWS AWSContext m a

initialRDSContext :: HTTP.Manager -> AWSContext
initialRDSContext mgr = AWSContext
    { manager = mgr
    , endpoint = "rds.amazonaws.com"
    , lastRequestId = Nothing
    }

runRDS :: MonadIO m => Credential -> RDS m a -> m a
runRDS = runAWS initialRDSContext

describeDBInstances
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ DBInstanceIdentifier
    -> Maybe Int -- ^ MaxRecords
    -> Maybe Text -- ^ Marker
    -> RDS m [DBInstance]
describeDBInstances dbid maxRecords marker = do
    ctx <- State.get
    cred <- Reader.ask
    rs <- lift $ requestQuery cred ctx (textToBS action) params apiVersion undefined
--    lift $ rs $$+- CB.sinkFile "debug.txt" >> fail "debug"
    (res, rid) <- lift $ rs $$+- XML.parseBytes XML.def =$ do
        sinkResponse action $ sinkDBInstances
    State.put ctx { lastRequestId = Just rid }
    return res
  where
    action = "DescribeDBInstances"
    mk name = maybe [] (\a -> [ValueParam name a])
    params = uncurry mk =<<
        [ ("DBInstanceIdentifier", dbid)
        , ("MaxRecords", toText <$> maxRecords)
        , ("Marker", marker)
        ]

elements :: MonadThrow m
    => Text
    -> GLSink Event m a
    -> GLSink Event m [a]
elements name f = element (name <> "s") $ listConsumer name f

sinkDBInstances
    :: MonadThrow m
    => GLSink Event m [DBInstance]
sinkDBInstances = elements "DBInstance" $
    DBInstance
    <$> getM "Iops" (textToInt <$>)
    <*> getF "BackupRetentionPeriod" textToInt
    <*> getF "MultiAZ" textToBool
    <*> getT "DBInstanceStatus"
    <*> getT "DBInstanceIdentifier"
    <*> getT "PreferredBackupWindow"
    <*> getT "PreferredMaintenanceWindow"
    <*> elementM "OptionGroupMembership"
        (OptionGroupMembership
        <$> getT "OptionGroupName"
        <*> getT "Status"
        )
    <*> getT "AvailabilityZone"
    <*> getM "LatestRestorableTime" (textToTime <$>)
    <*> elements "ReadReplicaDBInstanceIdentifier" text
    <*> getT "Engine"
    <*> sinkPendingModifiedValues
    <*> getMT "CharacterSetName"
    <*> getT "LicenseModel"
    <*> elementM "DBSubnetGroup"
        (DBSubnetGroup
        <$> getT "VpcId"
        <*> getT "SubnetGroupStatus"
        <*> getT "DBSubnetGroupDescription"
        <*> getT "DBSubnetGroupName"
        <*> elements "Subnet"
            (Subnet
            <$> getT "SubnetStatus"
            <*> getT "SubnetIdentifier"
            <*> element "SubnetAvailabilityZone"
                (AvailabilityZone
                <$> getT "Name"
                <*> getF "ProvisionedIopsCapable" textToBool
                )
            )
        )
    <*> elements "DBParameterGroup"
        (DBParameterGroupStatus
        <$> getT "ParameterApplyStatus"
        <*> getT "DBParameterGroupName"
        )
    <*> elementM "Endpoint"
        (Endpoint
        <$> getF "Port" textToInt
        <*> getT "Address"
        )
    <*> getT "EngineVersion"
    <*> getMT "ReadReplicaSourceDBInstanceIdentifier"
    <*> elements "DBSecurityGroup"
        (DBSecurityGroupMembership
        <$> getT "Status"
        <*> getT "DBSecurityGroupName"
        )
    <*> getMT "DBName"
    <*> getF "AutoMinorVersionUpgrade" textToBool
    <*> getM "InstanceCreateTime" (textToTime <$>)
    <*> getF "AllocatedStorage" textToInt
    <*> getT "DBInstanceClass"
    <*> getT "MasterUsername" 

sinkPendingModifiedValues
    :: MonadThrow m
    => GLSink Event m [PendingModifiedValue]
sinkPendingModifiedValues = element "PendingModifiedValues" $
    catMaybes <$> sequence [ m "MasterUserPassword" PMVMasterUserPassword
        , m "Iops" (PMVIops . textToInt)
        , m "MultiAZ" (PMVMultiAZ . textToBool)
        , m "AllocatedStorage" (PMVAllocatedStorage . textToInt)
        , m "EngineVersion" PMVEngineVersion
        , m "DBInstanceClass" PMVDBInstanceClass
        , m "BackupRetentionPeriod"
            (PMVBackupRetentionPeriod . textToInt)
        , m "Port" (PMVPort . textToInt)
        ]
  where
    m t f = getM t (f <$>)

sinkResponse
    :: MonadThrow m
    => Text -- ^ Action
    -> GLSink Event m a
    -> GLSink Event m (a, RequestId)
sinkResponse action sink = do
    sinkEventBeginDocument
    element (action <> "Response") $ (,)
        <$> element (action <> "Result") sink -- XXX: parse Marker
        <*> sinkResponseMetadata

sinkResponseMetadata
    :: MonadThrow m
    => GLSink Event m Text
sinkResponseMetadata =
    element "ResponseMetadata" $
        getT "RequestId"

sinkEventBeginDocument
    :: MonadThrow m
    => GLSink Event m ()
sinkEventBeginDocument = do
    me <- await
    case me of
        Nothing -> return ()
        Just EventBeginDocument -> return ()
        Just _ -> fail $ "unexpected: " <> show me

setRegion
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -> RDS m ()
setRegion region = do
    ctx <- State.get
    State.put
        ctx { endpoint =
            "rds." <> textToBS region <> ".amazonaws.com"
            }

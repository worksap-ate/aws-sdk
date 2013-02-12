{-# LANGUAGE FlexibleContexts #-}

module AWS.RDS.DBSecurityGroup
    ( describeDBSecurityGroups
    , createDBSecurityGroup
    , deleteDBSecurityGroup
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Conduit (GLSink, MonadBaseControl, MonadResource, MonadThrow)
import Data.Text (Text)
import Data.XML.Types (Event)

import AWS.Lib.Parser (getT, element)
import AWS.Lib.Query ((|=), (|=?))
import AWS.RDS.Internal (RDS, rdsQuery, rdsQueryOnlyMetadata, elements)
import AWS.RDS.Types hiding (Event)
import AWS.Util (toText)

describeDBSecurityGroups
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ DBSecurityGroupName
    -> Maybe Text -- ^ Marker
    -> Maybe Int -- ^ MaxRecords
    -> RDS m [DBSecurityGroup]
describeDBSecurityGroups name marker maxRecords =
    rdsQuery "DescribeDBSecurityGroups" params $
        elements "DBSecurityGroup" dbSecurityGroupSink
  where
    params =
        [ "DBSecurityGroupName" |=? name
        , "Marker" |=? marker
        , "MaxRecords" |=? toText <$> maxRecords
        ]

dbSecurityGroupSink
    :: MonadThrow m
    => GLSink Event m DBSecurityGroup
dbSecurityGroupSink = DBSecurityGroup
    <$> elements "EC2SecurityGroup" (
        EC2SecurityGroup
        <$> getT "Status"
        <*> getT "EC2SecurityGroupName"
        <*> getT "EC2SecurityGroupOwnerId"
        <*> getT "EC2SecurityGroupId"
        )
    <*> getT "DBSecurityGroupDescription"
    <*> elements "IPRange" (
        IPRange
        <$> getT "CIDRIP"
        <*> getT "Status"
        )
    <*> getT "VpcId"
    <*> getT "OwnerId"
    <*> getT "DBSecurityGroupName"

createDBSecurityGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBSecurityGroupName
    -> Text -- ^ DBSecurityGroupDescription
    -> RDS m DBSecurityGroup
createDBSecurityGroup name desc =
    rdsQuery "CreateDBSecurityGroup" params $
        element "DBSecurityGroup" dbSecurityGroupSink
  where
    params =
        [ "DBSecurityGroupName" |= name
        , "DBSecurityGroupDescription" |= desc
        ]

deleteDBSecurityGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBSecurityGroupName
    -> RDS m ()
deleteDBSecurityGroup name =
    rdsQueryOnlyMetadata "DeleteDBSecurityGroup"
        ["DBSecurityGroupName" |= name]

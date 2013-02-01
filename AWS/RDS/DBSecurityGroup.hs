{-# LANGUAGE FlexibleContexts #-}

module AWS.RDS.DBSecurityGroup
    ( describeDBSecurityGroups
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Conduit (GLSink, MonadBaseControl, MonadResource, MonadThrow)
import Data.Text (Text)
import Data.XML.Types (Event)

import AWS.Lib.Parser (getT)
import AWS.Lib.Query ((|=?))
import AWS.RDS.Internal (RDS, rdsQuery, elements)
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

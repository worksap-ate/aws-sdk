{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.RDS.DBSecurityGroup
    ( describeDBSecurityGroups
    , createDBSecurityGroup
    , deleteDBSecurityGroup
    , authorizeDBSecurityGroupIngress
    , revokeDBSecurityGroupIngress
    ) where

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<), element)
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
import Data.IP (AddrRange, IPv4)
import Data.Text (Text)

import Cloud.AWS.Lib.Query ((|=), (|=?))
import Cloud.AWS.RDS.Internal (RDS, rdsQueryOnlyMetadata, rdsQuery, elements)
import Cloud.AWS.RDS.Types hiding (Event)

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
        , "MaxRecords" |=? maxRecords
        ]

dbSecurityGroupSink
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m DBSecurityGroup
dbSecurityGroupSink xml = DBSecurityGroup
    <$> elements "EC2SecurityGroup" (\xml' ->
        EC2SecurityGroup
        <$> xml' .< "Status"
        <*> xml' .< "EC2SecurityGroupOwnerId"
        <*> xml' .< "EC2SecurityGroupName"
        <*> xml' .< "EC2SecurityGroupId"
        ) xml
    <*> xml .< "DBSecurityGroupDescription"
    <*> elements "IPRange" (\xml' ->
        IPRange
        <$> xml' .< "CIDRIP"
        <*> xml' .< "Status"
        ) xml
    <*> xml .< "VpcId"
    <*> xml .< "OwnerId"
    <*> xml .< "DBSecurityGroupName"

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

authorizeDBSecurityGroupIngress
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBSecurityGroupName
    -> Maybe (AddrRange IPv4) -- ^ CIDRIP
    -> Maybe Text -- ^ EC2SecurityGroupId
    -> Maybe Text -- ^ EC2SecurityGroupName
    -> Maybe Text -- ^ EC2SecurityGroupOwnerId
    -> RDS m DBSecurityGroup
authorizeDBSecurityGroupIngress dbsg ip sgid sgname sgoid =
    rdsQuery "AuthorizeDBSecurityGroupIngress" params $
        element "DBSecurityGroup" dbSecurityGroupSink
  where
    params =
        [ "DBSecurityGroupName" |= dbsg
        , "CIDRIP" |=? ip
        , "EC2SecurityGroupId" |=? sgid
        , "EC2SecurityGroupName" |=? sgname
        , "EC2SecurityGroupOwnerId" |=? sgoid
        ]

revokeDBSecurityGroupIngress
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBSecurityGroupName
    -> Maybe (AddrRange IPv4) -- ^ CIDRIP
    -> Maybe Text -- ^ EC2SecurityGroupId
    -> Maybe Text -- ^ EC2SecurityGroupName
    -> Maybe Text -- ^ EC2SecurityGroupOwnerId
    -> RDS m DBSecurityGroup
revokeDBSecurityGroupIngress dbsg ip sgid sgname sgoid =
    rdsQuery "RevokeDBSecurityGroupIngress" params $
        element "DBSecurityGroup" dbSecurityGroupSink
  where
    params =
        [ "DBSecurityGroupName" |= dbsg
        , "CIDRIP" |=? ip
        , "EC2SecurityGroupId" |=? sgid
        , "EC2SecurityGroupName" |=? sgname
        , "EC2SecurityGroupOwnerId" |=? sgoid
        ]

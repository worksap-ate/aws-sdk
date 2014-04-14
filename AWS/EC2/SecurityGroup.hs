{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}

module AWS.EC2.SecurityGroup
    ( describeSecurityGroups
    , createSecurityGroup
    , deleteSecurityGroup
    , authorizeSecurityGroupIngress
    , authorizeSecurityGroupEgress
    , revokeSecurityGroupIngress
    , revokeSecurityGroupEgress
    ) where

import Data.Text (Text)
import Data.ByteString (ByteString)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Applicative
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadBaseControl, MonadResource)
#endif

import AWS.EC2.Internal
import AWS.EC2.Types
import AWS.EC2.Query
import AWS.Lib.Parser
import AWS.Util

describeSecurityGroups
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ GroupNames
    -> [Text] -- ^ GroupIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m SecurityGroup)
describeSecurityGroups names ids filters =
    ec2QuerySource "DescribeSecurityGroups" params
    $ itemConduit "securityGroupInfo" $
        SecurityGroup
        <$> getT "ownerId"
        <*> getT "groupId"
        <*> getT "groupName"
        <*> getT "groupDescription"
        <*> getT "vpcId"
        <*> ipPermissionsSink "ipPermissions"
        <*> ipPermissionsSink "ipPermissionsEgress"
        <*> resourceTagSink
  where
    params =
        [ "GroupName" |.#= names
        , "GroupId" |.#= ids
        , filtersParam filters
        ]

ipPermissionsSink :: MonadThrow m
    => Text -> Consumer Event m [IpPermission]
ipPermissionsSink name = itemsSet name $ IpPermission
    <$> getT "ipProtocol"
    <*> getT "fromPort"
    <*> getT "toPort"
    <*> itemsSet "groups" (
        UserIdGroupPair
        <$> getT "userId"
        <*> getT "groupId"
        <*> getT "groupName"
        )
    <*> itemsSet "ipRanges" (getT "cidrIp")

createSecurityGroup
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ GroupName
    -> Text -- ^ GroupDescription
    -> Maybe Text -- ^ VpcId
    -> EC2 m (Maybe Text) -- ^ GroupId
createSecurityGroup name desc vpc =
    ec2Query "CreateSecurityGroup" params
        $ getT_ "return" *> getT "groupId"
  where
    params =
        [ "GroupName" |= name
        , "GroupDescription" |= desc
        , "VpcId" |=? vpc
        ]

deleteSecurityGroup
    :: (MonadResource m, MonadBaseControl IO m)
    => SecurityGroupRequest
    -> EC2 m Bool
deleteSecurityGroup param =
    ec2Query "DeleteSecurityGroup" params $ getT "return"
  where
    params = [securityGroupRequestParam param]

securityGroupRequestParam :: SecurityGroupRequest -> QueryParam
securityGroupRequestParam (SecurityGroupRequestGroupId t) =
    "GroupId" |= t
securityGroupRequestParam (SecurityGroupRequestGroupName t) =
    "GroupName" |= t

authorizeSecurityGroupIngress
    :: (MonadResource m, MonadBaseControl IO m)
    => SecurityGroupRequest
    -> [IpPermission]
    -> EC2 m Bool
authorizeSecurityGroupIngress =
    securityGroupQuery "AuthorizeSecurityGroupIngress"

authorizeSecurityGroupEgress
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ GroupId
    -> [IpPermission]
    -> EC2 m Bool
authorizeSecurityGroupEgress gid =
    securityGroupQuery "AuthorizeSecurityGroupEgress"
        $ SecurityGroupRequestGroupId gid

revokeSecurityGroupIngress
    :: (MonadResource m, MonadBaseControl IO m)
    => SecurityGroupRequest
    -> [IpPermission]
    -> EC2 m Bool
revokeSecurityGroupIngress =
    securityGroupQuery "RevokeSecurityGroupIngress"

revokeSecurityGroupEgress
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ GroupId
    -> [IpPermission]
    -> EC2 m Bool
revokeSecurityGroupEgress gid =
    securityGroupQuery "RevokeSecurityGroupEgress"
        $ SecurityGroupRequestGroupId gid

securityGroupQuery
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString -- ^ Action
    -> SecurityGroupRequest
    -> [IpPermission]
    -> EC2 m Bool
securityGroupQuery act param ipps =
    ec2Query act params $ getT "return"
  where
    params =
        [ securityGroupRequestParam param
        , "IpPermissions" |.#. map ipPermissionParams ipps
        ]

ipPermissionParams :: IpPermission -> [QueryParam]
ipPermissionParams ipp =
    [ "IpProtocol" |= ipPermissionIpProtocol ipp
    , "FromPort" |=? toText <$> ipPermissionFromPort ipp
    , "ToPort" |=? toText <$> ipPermissionToPort ipp
    , "Groups" |.#. map groupPairParams (ipPermissionGroups ipp)
    , "IpRanges" |.#. map (\a -> ["CidrIp" |= toText a]) (ipPermissionIpRanges ipp)
    ]
  where
    groupPairParams gp =
        [ "UserId" |=? userIdGroupPairUserId gp
        , "GroupId" |=? userIdGroupPairGroupId gp
        , "GroupName" |=? userIdGroupPairGroupName gp
        ]

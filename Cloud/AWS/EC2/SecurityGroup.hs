{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.EC2.SecurityGroup
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

import Data.Conduit
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<))

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query

describeSecurityGroups
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ GroupNames
    -> [Text] -- ^ GroupIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m SecurityGroup)
describeSecurityGroups names ids filters =
    ec2QuerySource "DescribeSecurityGroups" params path
    $ itemConduit $ \xml ->
        SecurityGroup
        <$> xml .< "ownerId"
        <*> xml .< "groupId"
        <*> xml .< "groupName"
        <*> xml .< "groupDescription"
        <*> xml .< "vpcId"
        <*> ipPermissionsConv "ipPermissions" xml
        <*> ipPermissionsConv "ipPermissionsEgress" xml
        <*> resourceTagConv xml
  where
    path = itemsPath "securityGroupInfo"
    params =
        [ "GroupName" |.#= names
        , "GroupId" |.#= ids
        , filtersParam filters
        ]

ipPermissionsConv :: (MonadThrow m, Applicative m)
    => Text -> XmlElement -> m [IpPermission]
ipPermissionsConv name = itemsSet name conv
  where
    conv e = IpPermission
        <$> e .< "ipProtocol"
        <*> e .< "fromPort"
        <*> e .< "toPort"
        <*> itemsSet "groups" uidConv e
        <*> itemsSet "ipRanges" (.< "cidrIp") e
    uidConv e = UserIdGroupPair
        <$> e .< "userId"
        <*> e .< "groupId"
        <*> e .< "groupName"


createSecurityGroup
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ GroupName
    -> Text -- ^ GroupDescription
    -> Maybe Text -- ^ VpcId
    -> EC2 m (Maybe Text) -- ^ GroupId
createSecurityGroup name desc vpc =
    ec2Query "CreateSecurityGroup" params (.< "groupId")
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
    ec2Query "DeleteSecurityGroup" params (.< "return")
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
    ec2Query act params (.< "return")
  where
    params =
        [ securityGroupRequestParam param
        , "IpPermissions" |.#. map ipPermissionParams ipps
        ]

ipPermissionParams :: IpPermission -> [QueryParam]
ipPermissionParams ipp =
    [ "IpProtocol" |= ipPermissionIpProtocol ipp
    , "FromPort" |=? ipPermissionFromPort ipp
    , "ToPort" |=? ipPermissionToPort ipp
    , "Groups" |.#. map groupPairParams (ipPermissionGroups ipp)
    , "IpRanges" |.#. map (\a -> ["CidrIp" |= a]) (ipPermissionIpRanges ipp)
    ]
  where
    groupPairParams gp =
        [ "UserId" |=? userIdGroupPairUserId gp
        , "GroupId" |=? userIdGroupPairGroupId gp
        , "GroupName" |=? userIdGroupPairGroupName gp
        ]

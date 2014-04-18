{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.EC2.Acl
    ( describeNetworkAcls
    , createNetworkAcl
    , deleteNetworkAcl
    , replaceNetworkAclAssociation
    , createNetworkAclEntry
    , deleteNetworkAclEntry
    , replaceNetworkAclEntry
    ) where

import Data.Text (Text)
import Data.Conduit
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<), elementM, element)

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query

describeNetworkAcls
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ NetworkAclId
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m NetworkAcl)
describeNetworkAcls nids filters = do
    ec2QuerySource "DescribeNetworkAcls" params path $
        itemConduit networkAclConv
  where
    path = itemsPath "networkAclSet"
    params =
        [ "NetworkAclId" |.#= nids
        , filtersParam filters
        ]

networkAclConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m NetworkAcl
networkAclConv = conv
  where
    conv e = NetworkAcl
        <$> e .< "networkAclId"
        <*> e .< "vpcId"
        <*> e .< "default"
        <*> itemsSet "entrySet" entryConv e
        <*> itemsSet "associationSet" assocConv e
        <*> resourceTagConv e
    entryConv e = NetworkAclEntry
        <$> e .< "ruleNumber"
        <*> e .< "protocol"
        <*> e .< "ruleAction"
        <*> e .< "egress"
        <*> e .< "cidrBlock"
        <*> elementM "icmpTypeCode" icmpConv e
        <*> elementM "portRange" prConv e
    icmpConv e = IcmpTypeCode
        <$> e .< "code"
        <*> e .< "type"
    prConv e = PortRange
        <$> e .< "from"
        <*> e .< "to"
    assocConv e = NetworkAclAssociation
        <$> e .< "networkAclAssociationId"
        <*> e .< "networkAclId"
        <*> e .< "subnetId"

createNetworkAcl
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VpcId
    -> EC2 m NetworkAcl
createNetworkAcl vpcid =
    ec2Query "CreateNetworkAcl" params $
        element "networkAcl" networkAclConv
  where
    params = ["VpcId" |= vpcid]

deleteNetworkAcl
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ NetworkAclId
    -> EC2 m Bool
deleteNetworkAcl = ec2Delete "DeleteNetworkAcl" "NetworkAclId"

replaceNetworkAclAssociation
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ AssociationId
    -> Text -- ^ NetworkAclId
    -> EC2 m Text
replaceNetworkAclAssociation assoc aclid =
    ec2Query "ReplaceNetworkAclAssociation" params (.< "newAssociationId")
  where
    params =
        [ "AssociationId" |= assoc
        , "NetworkAclId" |= aclid
        ]

createNetworkAclEntry
    :: (MonadResource m, MonadBaseControl IO m)
    => NetworkAclEntryRequest
    -> EC2 m Bool
createNetworkAclEntry req =
    ec2Query "CreateNetworkAclEntry" params (.< "return")
  where
    params = reqToParams req

reqToParams :: NetworkAclEntryRequest -> [QueryParam]
reqToParams req =
        [ "NetworkAclId" |=
            networkAclEntryRequestNetworkAclId req
        , "RuleNumber" |=
            networkAclEntryRequestRuleNumber req
        , "Protocol" |=
            networkAclEntryRequestProtocol req
        , "RuleAction" |=
            networkAclEntryRequestRuleAction req
        , "CidrBlock" |=
            networkAclEntryRequestCidrBlock req
        , "Egress" |=
            networkAclEntryRequestEgress req
        , "Icmp" |.? icmpParams <$> networkAclEntryRequestIcmp req
        , "PortRange" |.?
             portRangeParams <$> networkAclEntryRequestPortRange req
        ]
  where
    icmpParams icmp =
        [ "Code" |= icmpTypeCodeCode icmp
        , "Type" |= icmpTypeCodeType icmp
        ]
    portRangeParams pr =
        [ "From" |= portRangeFrom pr
        , "To" |= portRangeTo pr
        ]

deleteNetworkAclEntry
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ NetworkAclId
    -> Int -- ^ RuleNumber
    -> Bool -- ^ Egress
    -> EC2 m Bool
deleteNetworkAclEntry aclid rule egress =
    ec2Query "DeleteNetworkAclEntry" params (.< "return")
  where
    params =
        [ "NetworkAclId" |= aclid
        , "RuleNumber" |= rule
        , "Egress" |= egress
        ]

replaceNetworkAclEntry
    :: (MonadResource m, MonadBaseControl IO m)
    => NetworkAclEntryRequest
    -> EC2 m Bool
replaceNetworkAclEntry req =
    ec2Query "ReplaceNetworkAclEntry" params (.< "return")
  where
    params = reqToParams req

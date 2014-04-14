{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}

module AWS.EC2.Acl
    ( describeNetworkAcls
    , createNetworkAcl
    , deleteNetworkAcl
    , replaceNetworkAclAssociation
    , createNetworkAclEntry
    , deleteNetworkAclEntry
    , replaceNetworkAclEntry
    ) where

import Data.Text (Text)
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

describeNetworkAcls
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ NetworkAclId
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m NetworkAcl)
describeNetworkAcls nids filters = do
    ec2QuerySource "DescribeNetworkAcls" params $
        itemConduit "networkAclSet" networkAclSink
  where
    params =
        [ "NetworkAclId" |.#= nids
        , filtersParam filters
        ]

networkAclSink :: MonadThrow m
    => Consumer Event m NetworkAcl
networkAclSink = NetworkAcl
    <$> getT "networkAclId"
    <*> getT "vpcId"
    <*> getT "default"
    <*> itemsSet "entrySet" (NetworkAclEntry
        <$> getT "ruleNumber"
        <*> getT "protocol"
        <*> getT "ruleAction"
        <*> getT "egress"
        <*> getT "cidrBlock"
        <*> elementM "icmpTypeCode" (IcmpTypeCode
            <$> getT "code"
            <*> getT "type"
            )
        <*> elementM "portRange" (PortRange
            <$> getT "from"
            <*> getT "to"))
    <*> itemsSet "associationSet" (NetworkAclAssociation
        <$> getT "networkAclAssociationId"
        <*> getT "networkAclId"
        <*> getT "subnetId"
        )
    <*> resourceTagSink

createNetworkAcl
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VpcId
    -> EC2 m NetworkAcl
createNetworkAcl vpcid =
    ec2Query "CreateNetworkAcl" params $
        element "networkAcl" networkAclSink
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
    ec2Query "ReplaceNetworkAclAssociation" params $
        getT "newAssociationId"
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
    ec2Query "CreateNetworkAclEntry" params $ getT "return"
  where
    params = reqToParams req

reqToParams :: NetworkAclEntryRequest -> [QueryParam]
reqToParams req =
        [ "NetworkAclId" |=
            networkAclEntryRequestNetworkAclId req
        , "RuleNumber" |=
            toText (networkAclEntryRequestRuleNumber req)
        , "Protocol" |=
            toText (networkAclEntryRequestProtocol req)
        , "RuleAction" |=
            ruleToText (networkAclEntryRequestRuleAction req)
        , "CidrBlock" |=
            toText (networkAclEntryRequestCidrBlock req)
        , "Egress" |=
            boolToText (networkAclEntryRequestEgress req)
        , "Icmp" |.? icmpParams <$> networkAclEntryRequestIcmp req
        , "PortRange" |.?
             portRangeParams <$> networkAclEntryRequestPortRange req
        ]
  where
    ruleToText NetworkAclRuleActionAllow = "allow"
    ruleToText NetworkAclRuleActionDeny = "deny"
    icmpParams icmp =
        [ "Code" |= toText (icmpTypeCodeCode icmp)
        , "Type" |= toText (icmpTypeCodeType icmp)
        ]
    portRangeParams pr =
        [ "From" |= toText (portRangeFrom pr)
        , "To" |= toText (portRangeTo pr)
        ]

deleteNetworkAclEntry
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ NetworkAclId
    -> Int -- ^ RuleNumber
    -> Bool -- ^ Egress
    -> EC2 m Bool
deleteNetworkAclEntry aclid rule egress =
    ec2Query "DeleteNetworkAclEntry" params $ getT "return"
  where
    params =
        [ "NetworkAclId" |= aclid
        , "RuleNumber" |= toText rule
        , "Egress" |= boolToText egress
        ]

replaceNetworkAclEntry
    :: (MonadResource m, MonadBaseControl IO m)
    => NetworkAclEntryRequest
    -> EC2 m Bool
replaceNetworkAclEntry req =
    ec2Query "ReplaceNetworkAclEntry" params $ getT "return"
  where
    params = reqToParams req

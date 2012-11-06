{-# LANGUAGE FlexibleContexts, RankNTypes #-}

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
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Convert
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
        [ ArrayParams "NetworkAclId" nids
        , FilterParams filters
        ]

networkAclSink :: MonadThrow m
    => GLSink Event m NetworkAcl
networkAclSink = NetworkAcl
    <$> getT "networkAclId"
    <*> getT "vpcId"
    <*> getF "default" textToBool
    <*> itemsSet "entrySet" (NetworkAclEntry
        <$> getF "ruleNumber" textToInt
        <*> getF "protocol" textToInt
        <*> getF "ruleAction" networkAclRuleAction
        <*> getF "egress" textToBool
        <*> getT "cidrBlock"
        <*> elementM "icmpTypeCode" (IcmpTypeCode
            <$> getF "code" textToInt
            <*> getF "type" textToInt
            )
        <*> elementM "portRange" (PortRange
            <$> getF "from" textToInt
            <*> getF "to" textToInt))
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
    params = [ValueParam "VpcId" vpcid]

deleteNetworkAcl
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ NetworkAclId
    -> EC2 m Bool
deleteNetworkAcl aclid =
    ec2Query "DeleteNetworkAcl" params returnBool
  where
    params = [ValueParam "NetworkAclId" aclid]

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
        [ ValueParam "AssociationId" assoc
        , ValueParam "NetworkAclId" aclid
        ]

createNetworkAclEntry
    :: (MonadResource m, MonadBaseControl IO m)
    => NetworkAclEntryRequest
    -> EC2 m Bool
createNetworkAclEntry req =
    ec2Query "CreateNetworkAclEntry" params returnBool
  where
    params = reqToParams req

reqToParams :: NetworkAclEntryRequest -> [QueryParam]
reqToParams req =
        [ ValueParam "NetworkAclId" $
            networkAclEntryRequestNetworkAclId req
        , ValueParam "RuleNumber" $
            toText $ networkAclEntryRequestRuleNumber req
        , ValueParam "Protocol" $
            toText $ networkAclEntryRequestProtocol req
        , ValueParam "RuleAction" $
            ruleToText $ networkAclEntryRequestRuleAction req
        , ValueParam "CidrBlock" $
            networkAclEntryRequestCidrBlock req
        , ValueParam "Egress" $
            boolToText $ networkAclEntryRequestEgress req
        ] ++ maybeParams
        [ ("Icmp.Code",
            toText
            <$> icmpTypeCodeCode
            <$> networkAclEntryRequestIcmp req)
        , ("Icmp.Type",
            toText
            <$> icmpTypeCodeType
            <$> networkAclEntryRequestIcmp req)
        , ("PortRange.From",
            toText
            <$> portRangeFrom
            <$> networkAclEntryRequestPortRange req)
        , ("PortRange.To",
            toText
            <$> portRangeTo
            <$> networkAclEntryRequestPortRange req)
        ]
  where
    ruleToText NetworkAclRuleActionAllow = "allow"
    ruleToText NetworkAclRuleActionDeny = "deny"

deleteNetworkAclEntry
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ NetworkAclId
    -> Int -- ^ RuleNumber
    -> Bool -- ^ Egress
    -> EC2 m Bool
deleteNetworkAclEntry aclid rule egress =
    ec2Query "DeleteNetworkAclEntry" params returnBool
  where
    params =
        [ ValueParam "NetworkAclId" aclid
        , ValueParam "RuleNumber" $ toText rule
        , ValueParam "Egress" $ boolToText egress
        ]

replaceNetworkAclEntry
    :: (MonadResource m, MonadBaseControl IO m)
    => NetworkAclEntryRequest
    -> EC2 m Bool
replaceNetworkAclEntry req =
    ec2Query "ReplaceNetworkAclEntry" params returnBool
  where
    params = reqToParams req

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

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query
import Cloud.AWS.Lib.Parser.Unordered

describeNetworkAcls
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ NetworkAclId
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m NetworkAcl)
describeNetworkAcls nids filters = do
    ec2QuerySource "DescribeNetworkAcls" params $
        xmlParserConduit "networkAclSet" $ \xml ->
            getElement xml "item" networkAclConv
  where
    params =
        [ "NetworkAclId" |.#= nids
        , filtersParam filters
        ]

networkAclConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m NetworkAcl
networkAclConv xml = NetworkAcl
    <$> xml .< "networkAclId"
    <*> xml .< "vpcId"
    <*> xml .< "default"
    <*> getElements xml "entrySet" "item" (\xml' -> NetworkAclEntry
        <$> xml' .< "ruleNumber"
        <*> xml' .< "protocol"
        <*> xml' .< "ruleAction"
        <*> xml' .< "egress"
        <*> xml' .< "cidrBlock"
        <*> getElementM xml' "icmpTypeCode" (\xml'' -> IcmpTypeCode
            <$> xml'' .< "code"
            <*> xml'' .< "type"
            )
        <*> getElementM xml' "portRange" (\xml'' -> PortRange
            <$> xml'' .< "from"
            <*> xml'' .< "to"))
    <*> getElements xml "associationSet" "item" (\xml' -> NetworkAclAssociation
        <$> xml' .< "networkAclAssociationId"
        <*> xml' .< "networkAclId"
        <*> xml' .< "subnetId"
        )
    <*> resourceTagConv xml

createNetworkAcl
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VpcId
    -> EC2 m NetworkAcl
createNetworkAcl vpcid =
    ec2Query "CreateNetworkAcl" params $ xmlParser $ \xml ->
        getElement xml "networkAcl" networkAclConv
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
        xmlParser (.< "newAssociationId")
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
    ec2Query "CreateNetworkAclEntry" params $ xmlParser (.< "return")
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
    ec2Query "DeleteNetworkAclEntry" params $ xmlParser (.< "return")
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
    ec2Query "ReplaceNetworkAclEntry" params $ xmlParser (.< "return")
  where
    params = reqToParams req

{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.RDS.Internal
    ( apiVersion
    , RDS
    , rdsQuery
    , rdsQueryOnlyMetadata
    , dbSubnetGroupSink
    , dbSecurityGroupMembershipSink
    , vpcSecurityGroupMembershipSink
    , elements
    , elements'
    ) where

import Control.Applicative
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Conduit
import Data.Monoid ((<>))
import qualified Text.XML.Stream.Parse as XmlP

import Cloud.AWS.Class
import Cloud.AWS.Lib.Query
import Cloud.AWS.Lib.Parser
import Cloud.AWS.Lib.Parser.Unordered (SimpleXML, xmlParser, getElement, getElements, (.<))
import Cloud.AWS.Lib.ToText (toText)
import Cloud.AWS.RDS.Types hiding (Event)

-- | Ver.2013-01-10
apiVersion :: ByteString
apiVersion = "2013-02-12"

type RDS m a = AWS AWSContext m a

rdsQuery
    :: (MonadBaseControl IO m, MonadResource m)
    => ByteString -- ^ Action
    -> [QueryParam]
    -> (SimpleXML -> m a)
    -> RDS m a
rdsQuery = commonQuery apiVersion

rdsQueryOnlyMetadata
    :: (MonadBaseControl IO m, MonadResource m)
    => ByteString
    -> [QueryParam]
    -> RDS m ()
rdsQueryOnlyMetadata action params = do
    ctx <- State.get
    settings <- Reader.ask
    rs <- lift $ requestQuery settings ctx action params apiVersion sinkError
    rid <- lift $ rs
        $$+- XmlP.parseBytes XmlP.def
        =$ xmlParser (sinkResponseOnlyMetadata (toText action))
    State.put ctx { lastRequestId = Just rid }
    return ()

sinkResponseOnlyMetadata
    :: (MonadThrow m, Applicative m)
    => Text
    -> SimpleXML
    -> m RequestId
sinkResponseOnlyMetadata action xml = do
    getElement xml (action <> "Response") sinkResponseMetadata

elements :: (MonadThrow m, Applicative m)
    => Text
    -> (SimpleXML -> m a)
    -> SimpleXML
    -> m [a]
elements name = elements' (name <> "s") name

elements' :: forall m a . (MonadThrow m, Applicative m)
    => Text
    -> Text
    -> (SimpleXML -> m a)
    -> SimpleXML
    -> m [a]
elements' setName itemName inner xml =
    getElements xml setName itemName inner

dbSubnetGroupSink
    :: (MonadThrow m, Applicative m)
    => SimpleXML -> m DBSubnetGroup
dbSubnetGroupSink xml = DBSubnetGroup
    <$> xml .< "VpcId"
    <*> xml .< "SubnetGroupStatus"
    <*> xml .< "DBSubnetGroupDescription"
    <*> xml .< "DBSubnetGroupName"
    <*> elements "Subnet" (\xml' ->
        Subnet
        <$> xml' .< "SubnetStatus"
        <*> xml' .< "SubnetIdentifier"
        <*> getElement xml' "SubnetAvailabilityZone" (\xml'' ->
            AvailabilityZone
            <$> xml'' .< "Name"
            <*> xml'' .< "ProvisionedIopsCapable"
            )
        ) xml

dbSecurityGroupMembershipSink
    :: (MonadThrow m, Applicative m)
    => SimpleXML -> m DBSecurityGroupMembership
dbSecurityGroupMembershipSink xml = DBSecurityGroupMembership
    <$> xml .< "Status"
    <*> xml .< "DBSecurityGroupName"

vpcSecurityGroupMembershipSink
    :: (MonadThrow m, Applicative m)
    => SimpleXML -> m VpcSecurityGroupMembership
vpcSecurityGroupMembershipSink xml = VpcSecurityGroupMembership
    <$> xml .< "Status"
    <*> xml .< "VpcSecurityGroupId"

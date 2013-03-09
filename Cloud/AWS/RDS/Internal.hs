{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.RDS.Internal
    ( apiVersion
    , RDS
    , rdsQuery
    , rdsQueryOnlyMetadata
    , elements
    , elements'
    , dbSubnetGroupSink
    , dbSecurityGroupMembershipSink
    , vpcSecurityGroupMembershipSink
    ) where

import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Conduit
import Data.Monoid ((<>))
import Data.XML.Types (Event(..))
import Data.Maybe (fromMaybe)
import qualified Text.XML.Stream.Parse as XmlP

import Cloud.AWS.Class
import Cloud.AWS.Lib.Query
import Cloud.AWS.Lib.Parser
import Cloud.AWS.Lib.ToText (toText)
import Cloud.AWS.RDS.Types hiding (Event)

-- | Ver.2013-01-10
apiVersion :: ByteString
apiVersion = "2013-01-10"

type RDS m a = AWS AWSContext m a

rdsQuery
    :: (MonadBaseControl IO m, MonadResource m)
    => ByteString -- ^ Action
    -> [QueryParam]
    -> Consumer Event m a
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
    rid <- lift $ rs $$+-
        XmlP.parseBytes XmlP.def =$
            sinkResponseOnlyMetadata (toText action)
    State.put ctx { lastRequestId = Just rid }
    return ()

sinkResponseOnlyMetadata
    :: MonadThrow m
    => Text
    -> Consumer Event m RequestId
sinkResponseOnlyMetadata action = do
    sinkEventBeginDocument
    element (action <> "Response") $ sinkResponseMetadata

elements :: MonadThrow m
    => Text
    -> Consumer Event m a
    -> Consumer Event m [a]
elements name = elements' (name <> "s") name

elements' :: forall m a . MonadThrow m
    => Text
    -> Text
    -> Consumer Event m a
    -> Consumer Event m [a]
elements' setName itemName inner =
    fromMaybe [] <$> elementM setName (listConsumer itemName inner)

dbSubnetGroupSink
    :: MonadThrow m
    => Consumer Event m DBSubnetGroup
dbSubnetGroupSink = DBSubnetGroup
    <$> getT "VpcId"
    <*> getT "SubnetGroupStatus"
    <*> getT "DBSubnetGroupDescription"
    <*> getT "DBSubnetGroupName"
    <*> elements "Subnet" (
        Subnet
        <$> getT "SubnetStatus"
        <*> getT "SubnetIdentifier"
        <*> element "SubnetAvailabilityZone" (
            AvailabilityZone
            <$> getT "Name"
            <*> getT "ProvisionedIopsCapable"
            )
        )

dbSecurityGroupMembershipSink
    :: MonadThrow m
    => Consumer Event m DBSecurityGroupMembership
dbSecurityGroupMembershipSink = DBSecurityGroupMembership
    <$> getT "Status"
    <*> getT "DBSecurityGroupName"

vpcSecurityGroupMembershipSink
    :: MonadThrow m
    => Consumer Event m VpcSecurityGroupMembership
vpcSecurityGroupMembershipSink = VpcSecurityGroupMembership
    <$> getT "Status"
    <*> getT "VpcSecurityGroupId"

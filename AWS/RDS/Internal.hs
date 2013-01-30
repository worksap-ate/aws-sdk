{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}

module AWS.RDS.Internal
    ( apiVersion
    , RDS
    , rdsQuery
    , elements
    , elements'
#ifdef DEBUG
    , rdsQueryDebug
#endif
    , dbSubnetGroupSink
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Conduit
import Data.Monoid ((<>))
import Data.XML.Types (Event(..))
import Data.Maybe (fromMaybe)

import AWS.Class
import AWS.Lib.Query
import AWS.Lib.Parser
import AWS.RDS.Types (DBSubnetGroup(..), Subnet(..), AvailabilityZone(..))

apiVersion :: ByteString
apiVersion = "2013-01-10"

type RDS m a = AWS AWSContext m a

rdsQuery
    :: (MonadBaseControl IO m, MonadResource m)
    => ByteString -- ^ Action
    -> [QueryParam]
    -> GLSink Event m a
    -> RDS m a
rdsQuery = commonQuery apiVersion

elements :: MonadThrow m
    => Text
    -> GLSink Event m a
    -> GLSink Event m [a]
elements name = elements' (name <> "s") name

elements' :: forall m a . MonadThrow m
    => Text
    -> Text
    -> GLSink Event m a
    -> GLSink Event m [a]
elements' setName itemName inner =
    fromMaybe [] <$> elementM setName (listConsumer itemName inner)

#ifdef DEBUG
rdsQueryDebug
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> RDS m a
rdsQueryDebug = debugQuery apiVersion
#endif

dbSubnetGroupSink
    :: MonadThrow m
    => GLSink Event m DBSubnetGroup
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

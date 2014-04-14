{-# LANGUAGE FlexibleContexts, CPP #-}

module AWS.RDS.DBParameterGroup
    ( describeDBParameterGroups
    , createDBParameterGroup
    , deleteDBParameterGroup
    ) where

import Control.Applicative ((<$>), (<*>))
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
import Data.Conduit (Consumer)
#else
import Data.Conduit (Consumer, MonadBaseControl, MonadResource, MonadThrow)
#endif
import Data.Text (Text)
import Data.XML.Types (Event)

import AWS.Lib.Parser (getT, element)
import AWS.Lib.Query ((|=), (|=?))
import AWS.RDS.Internal (RDS, rdsQuery, rdsQueryOnlyMetadata, elements)
import AWS.RDS.Types hiding (Event)
import AWS.Util (toText)

describeDBParameterGroups
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ DBParameterGroupName
    -> Maybe Text -- ^ Marker
    -> Maybe Int -- ^ MaxRecords
    -> RDS m [DBParameterGroup]
describeDBParameterGroups name marker maxRecords =
    rdsQuery "DescribeDBParameterGroups" params $
        elements "DBParameterGroup" dbParameterGroupSink
  where
    params =
        [ "DBParameterGroupName" |=? name
        , "Marker" |=? marker
        , "MaxRecords" |=? toText <$> maxRecords
        ]

dbParameterGroupSink
    :: MonadThrow m
    => Consumer Event m DBParameterGroup
dbParameterGroupSink = DBParameterGroup
    <$> getT "DBParameterGroupFamily"
    <*> getT "Description"
    <*> getT "DBParameterGroupName"

createDBParameterGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBParameterGroupFamily
    -> Text -- ^ DBParameterGroupName
    -> Text -- ^ Description
    -> RDS m DBParameterGroup
createDBParameterGroup family name desc =
    rdsQuery "CreateDBParameterGroup" params $
        element "DBParameterGroup" dbParameterGroupSink
  where
    params =
        [ "DBParameterGroupFamily" |= family
        , "DBParameterGroupName" |= name
        , "Description" |= desc
        ]

deleteDBParameterGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBParameterGroupName
    -> RDS m ()
deleteDBParameterGroup name =
    rdsQueryOnlyMetadata "DeleteDBParameterGroup"
        ["DBParameterGroupName" |= name]

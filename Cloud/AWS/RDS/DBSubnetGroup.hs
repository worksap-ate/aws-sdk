{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.RDS.DBSubnetGroup
    ( describeDBSubnetGroups
    , createDBSubnetGroup
    , deleteDBSubnetGroup
    , modifyDBSubnetGroup
    ) where

import Cloud.AWS.Lib.Parser.Unordered (element)
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Data.Text (Text)

import Cloud.AWS.Lib.Query ((|=), (|=?), (|.#=))
import Cloud.AWS.RDS.Internal
import Cloud.AWS.RDS.Types (DBSubnetGroup)

describeDBSubnetGroups
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ DBSubnetGroupName
    -> Maybe Text -- ^ Marker
    -> Maybe Int -- ^ MaxRecords
    -> RDS m [DBSubnetGroup]
describeDBSubnetGroups name marker maxRecords =
    rdsQuery "DescribeDBSubnetGroups" params $
        elements "DBSubnetGroup" dbSubnetGroupSink
  where
    params =
        [ "DBSubnetGroupName" |=? name
        , "Marker" |=? marker
        , "MaxRecords" |=? maxRecords
        ]

createDBSubnetGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBSubnetGroupName
    -> [Text] -- ^ SubnetIds
    -> Text -- ^ DBSubnetGroupDescription
    -> RDS m DBSubnetGroup
createDBSubnetGroup name ids desc =
    rdsQuery "CreateDBSubnetGroup" params $
        element "DBSubnetGroup" dbSubnetGroupSink
  where
    params =
        [ "DBSubnetGroupName" |= name
        , "SubnetIds.member" |.#= ids
        , "DBSubnetGroupDescription" |= desc
        ]

deleteDBSubnetGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBSubnetGroupName
    -> RDS m ()
deleteDBSubnetGroup name =
    rdsQueryOnlyMetadata "DeleteDBSubnetGroup"
        ["DBSubnetGroupName" |= name]

modifyDBSubnetGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBSubnetGroupName
    -> Maybe Text -- ^ DBSubnetGroupDescription
    -> [Text] -- ^ SubnetIds
    -> RDS m DBSubnetGroup
modifyDBSubnetGroup name desc ids =
    rdsQuery "ModifyDBSubnetGroup" params $
        element "DBSubnetGroup" dbSubnetGroupSink
  where
    params =
        [ "DBSubnetGroupName" |= name
        , "DBSubnetGroupDescription" |=? desc
        , "SubnetIds.member" |.#= ids
        ]

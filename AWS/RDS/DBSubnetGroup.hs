{-# LANGUAGE FlexibleContexts #-}

module AWS.RDS.DBSubnetGroup
    ( describeDBSubnetGroups
    , createDBSubnetGroup
    ) where

import Control.Applicative ((<$>))
import Data.Conduit
import Data.Text (Text)

import AWS.Lib.Parser (element)
import AWS.Lib.Query ((|=), (|=?), (|.#=))
import AWS.RDS.Internal
import AWS.RDS.Types (DBSubnetGroup)
import AWS.Util (toText)

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
        , "MaxRecords" |=? toText <$> maxRecords
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

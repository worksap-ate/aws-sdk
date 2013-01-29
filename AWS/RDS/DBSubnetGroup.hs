{-# LANGUAGE FlexibleContexts #-}

module AWS.RDS.DBSubnetGroup
    ( describeDBSubnetGroups
    ) where

import Control.Applicative ((<$>))
import Data.Conduit
import Data.Text (Text)

import AWS.Lib.Query ((|=?))
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

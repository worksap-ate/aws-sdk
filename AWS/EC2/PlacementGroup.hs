{-# LANGUAGE FlexibleContexts #-}
module AWS.EC2.PlacementGroup
    ( describePlacementGroups
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Conduit
import Data.Text (Text)
import Data.XML.Types (Event)

import AWS.EC2.Internal (EC2, itemConduit)
import AWS.EC2.Query (ec2QuerySource)
import AWS.EC2.Types (Filter, PlacementGroup(..))
import AWS.Lib.Parser (getT)
import AWS.Lib.Query ((|.#=), filtersParam)

describePlacementGroups
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ Placement group names.
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m PlacementGroup)
describePlacementGroups groupNames filters =
    ec2QuerySource "DescribePlacementGroups" params $
        itemConduit "placementGroupSet" placementGroupSink
  where
    params =
        [ "GroupName" |.#= groupNames
        , filtersParam filters
        ]

placementGroupSink :: MonadThrow m => GLSink Event m PlacementGroup
placementGroupSink =
    PlacementGroup
    <$> getT "groupName"
    <*> getT "strategy"
    <*> getT "state"

{-# LANGUAGE FlexibleContexts #-}
module Cloud.AWS.EC2.PlacementGroup
    ( describePlacementGroups
    , createPlacementGroup
    , deletePlacementGroup
    ) where

import Control.Applicative ((<$>), (<*>), Applicative)
import Data.Conduit
import Data.Text (Text)

import Cloud.AWS.EC2.Internal (EC2, itemConduit)
import Cloud.AWS.EC2.Query (ec2Query, ec2QuerySource)
import Cloud.AWS.EC2.Types (Filter, PlacementGroup(..), PlacementGroupStrategy(..))
import Cloud.AWS.Lib.Parser.Unordered ((.<), SimpleXML, xmlParser)
import Cloud.AWS.Lib.Query ((|=), (|.#=), filtersParam)

describePlacementGroups
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ Placement group names.
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m PlacementGroup)
describePlacementGroups groupNames filters =
    ec2QuerySource "DescribePlacementGroups" params $
        itemConduit "placementGroupSet" placementGroupConv
  where
    params =
        [ "GroupName" |.#= groupNames
        , filtersParam filters
        ]

placementGroupConv :: (MonadThrow m, Applicative m) => SimpleXML -> m PlacementGroup
placementGroupConv xml =
    PlacementGroup
    <$> xml .< "groupName"
    <*> xml .< "strategy"
    <*> xml .< "state"

createPlacementGroup
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ A name for the placement group.
    -> PlacementGroupStrategy -- ^ The placement group strategy.
    -> EC2 m Bool
createPlacementGroup groupName strategy =
    ec2Query "CreatePlacementGroup" params $ xmlParser (.< "return")
  where
    params =
        [ "GroupName" |= groupName
        , "Strategy" |= stringifyStrategy strategy
        ]

stringifyStrategy :: PlacementGroupStrategy -> Text
stringifyStrategy PlacementGroupStrategyCluster = "cluster"

deletePlacementGroup
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ The name of the placement group.
    -> EC2 m Bool
deletePlacementGroup groupName =
    ec2Query "DeletePlacementGroup" ["GroupName" |= groupName] $ xmlParser (.< "return")

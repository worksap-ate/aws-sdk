{-# LANGUAGE FlexibleContexts, CPP #-}
module AWS.EC2.PlacementGroup
    ( describePlacementGroups
    , createPlacementGroup
    , deletePlacementGroup
    ) where

import Control.Applicative ((<$>), (<*>))
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadBaseControl, MonadResource)
#endif
import Data.Conduit
import Data.Text (Text)
import Data.XML.Types (Event)

import AWS.EC2.Internal (EC2, itemConduit)
import AWS.EC2.Query (ec2Query, ec2QuerySource)
import AWS.EC2.Types (Filter, PlacementGroup(..), PlacementGroupStrategy(..))
import AWS.Lib.Parser (getT)
import AWS.Lib.Query ((|=), (|.#=), filtersParam)

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

placementGroupSink :: MonadThrow m => Consumer Event m PlacementGroup
placementGroupSink =
    PlacementGroup
    <$> getT "groupName"
    <*> getT "strategy"
    <*> getT "state"

createPlacementGroup
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ A name for the placement group.
    -> PlacementGroupStrategy -- ^ The placement group strategy.
    -> EC2 m Bool
createPlacementGroup groupName strategy =
    ec2Query "CreatePlacementGroup" params $ getT "return"
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
    ec2Query "DeletePlacementGroup" ["GroupName" |= groupName] $ getT "return"

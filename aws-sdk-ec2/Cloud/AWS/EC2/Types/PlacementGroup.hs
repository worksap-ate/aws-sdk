{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.EC2.Types.PlacementGroup
    ( PlacementGroup(..)
    , PlacementGroupState(..)
    , PlacementGroupStrategy(..)
    ) where

import Cloud.AWS.Lib.FromText (deriveFromText)
import Data.Text (Text)

data PlacementGroup = PlacementGroup
    { placementGroupGroupName :: Text
    , placementGroupStrategy :: PlacementGroupStrategy
    , placementGroupState :: PlacementGroupState
    }
  deriving (Show, Read, Eq)

data PlacementGroupState
    = PlacementGroupStatePending
    | PlacementGroupStateAvailable
    | PlacementGroupStateDeleting
    | PlacementGroupStateDeleted
  deriving (Show, Read, Eq)

data PlacementGroupStrategy
    = PlacementGroupStrategyCluster
  deriving (Show, Read, Eq)

deriveFromText "PlacementGroupState"
    ["pending", "available", "deleting", "deleted"]
deriveFromText "PlacementGroupStrategy" ["cluster"]

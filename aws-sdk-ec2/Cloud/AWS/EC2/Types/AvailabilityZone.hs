module Cloud.AWS.EC2.Types.AvailabilityZone
    ( AvailabilityZone(..)
    , AvailabilityZoneMessage
    ) where

import Data.Text (Text)

data AvailabilityZone = AvailabilityZone
    { zoneName :: Text
    , zoneState :: Text
    , zoneRegionName :: Text
    , zoneMessageSet :: [AvailabilityZoneMessage]
    }
  deriving (Show, Read, Eq)

type AvailabilityZoneMessage = Text

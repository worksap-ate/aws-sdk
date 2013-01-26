module AWS.EC2.Types.Region
    ( Region(..)
    ) where

import AWS.Lib.FromText

data Region = Region
    { regionName :: Text
    , regionEndpoint :: Text
    }
  deriving (Show, Read, Eq)

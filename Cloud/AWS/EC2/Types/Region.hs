module Cloud.AWS.EC2.Types.Region
    ( Region(..)
    ) where

import Cloud.AWS.Lib.FromText

data Region = Region
    { regionName :: Text
    , regionEndpoint :: Text
    }
  deriving (Show, Read, Eq)

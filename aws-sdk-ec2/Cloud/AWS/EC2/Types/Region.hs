module Cloud.AWS.EC2.Types.Region
    ( Region(..)
    ) where

import Data.Text (Text)

data Region = Region
    { regionName :: Text
    , regionEndpoint :: Text
    }
  deriving (Show, Read, Eq)

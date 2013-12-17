module Cloud.AWS.RDS.Types.Tag
    ( Tag(..)
    ) where

import Data.Text (Text)

data Tag = Tag
    { tagValue :: Text
    , tagKey :: Text
    }
  deriving (Show, Eq)

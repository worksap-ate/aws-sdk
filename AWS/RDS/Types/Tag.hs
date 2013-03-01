module AWS.RDS.Types.Tag
    ( Tag(..)
    ) where

import AWS.Lib.FromText (Text)

data Tag = Tag
    { tagValue :: Text
    , tagKey :: Text
    }
  deriving (Show, Eq)

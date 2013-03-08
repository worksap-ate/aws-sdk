module Cloud.AWS.RDS.Types.Tag
    ( Tag(..)
    ) where

import Cloud.AWS.Lib.FromText (Text)

data Tag = Tag
    { tagValue :: Text
    , tagKey :: Text
    }
  deriving (Show, Eq)

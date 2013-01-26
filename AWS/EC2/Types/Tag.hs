module AWS.EC2.Types.Tag
    ( Tag(..)
    ) where

import AWS.Lib.FromText

data Tag = Tag
    { tagResourceId :: Text
    , tagResourceType :: Text
    , tagKey :: Text
    , tagValue :: Maybe Text
    }
  deriving (Show, Read, Eq)

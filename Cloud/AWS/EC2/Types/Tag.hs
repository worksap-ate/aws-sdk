module Cloud.AWS.EC2.Types.Tag
    ( Tag(..)
    ) where

import Cloud.AWS.Lib.FromText

data Tag = Tag
    { tagResourceId :: Text
    , tagResourceType :: Text
    , tagKey :: Text
    , tagValue :: Maybe Text
    }
  deriving (Show, Read, Eq)

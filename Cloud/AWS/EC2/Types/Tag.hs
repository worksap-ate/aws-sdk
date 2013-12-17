module Cloud.AWS.EC2.Types.Tag
    ( Tag(..)
    ) where

import Data.Text (Text)

data Tag = Tag
    { tagResourceId :: Text
    , tagResourceType :: Text
    , tagKey :: Text
    , tagValue :: Maybe Text
    }
  deriving (Show, Read, Eq)

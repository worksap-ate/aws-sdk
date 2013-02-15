module AWS.RDS.Types.DBParameterGroup
    ( DBParameterGroup(..)
    ) where

import AWS.Lib.FromText (Text)

data DBParameterGroup = DBParameterGroup
    { dbParameterGroupFamily :: Text
    , dbParameterGroupDescription :: Text
    , dbParameterGroupName :: Text
    }
  deriving (Show, Eq)

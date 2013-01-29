module AWS.RDS.Types.DBParameterGroup
    ( DBParameterGroup(..)
    ) where

import AWS.Lib.FromText (Text)

data DBParameterGroup = DBParameterGroup
    { dbpgDBParameterGroupFamily :: Text
    , dbpgDescription :: Text
    , dbpgDBParameterGroupName :: Text
    }
  deriving (Show, Eq)

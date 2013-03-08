module Cloud.AWS.EC2.Types.KeyPair
    ( KeyPair(..)
    ) where

import Cloud.AWS.Lib.FromText

data KeyPair = KeyPair
    { keyPairName :: Text
    , keyPairFingerprint :: Text
    }
  deriving (Show, Read, Eq)

module AWS.EC2.Types.KeyPair
    ( KeyPair(..)
    ) where

import AWS.Lib.FromText

data KeyPair = KeyPair
    { keyPairName :: Text
    , keyPairFingerprint :: Text
    }
  deriving (Show, Read, Eq)

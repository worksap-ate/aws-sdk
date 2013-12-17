module Cloud.AWS.EC2.Types.KeyPair
    ( KeyPair(..)
    ) where

import Data.Text (Text)

data KeyPair = KeyPair
    { keyPairName :: Text
    , keyPairFingerprint :: Text
    }
  deriving (Show, Read, Eq)

module AWS.EC2.Internal
    ( EC2Context(..)
    , EC2
    , runEC2
      -- re-export
    , AWSException(..)
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Network.HTTP.Conduit as HTTP
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()

import AWS.Class
import AWS.Credential

data EC2Context = EC2Context
    { manager :: HTTP.Manager
    , endpoint :: ByteString
    , lastRequestId :: Maybe Text
    }

initialEC2Context :: HTTP.Manager -> EC2Context
initialEC2Context mgr = EC2Context
    { manager = mgr
    , endpoint = "ec2.amazonaws.com"
    , lastRequestId = Nothing
    }

type EC2 m a = AWS EC2Context m a

runEC2 :: MonadIO m => Credential -> AWS EC2Context m a -> m a
runEC2 = runAWS initialEC2Context

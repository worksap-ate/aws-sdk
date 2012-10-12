module AWS.EC2.Internal
    ( AWSContext(..)
    , EC2
    , runEC2
      -- re-export
    , AWSException(..)
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Network.HTTP.Conduit as HTTP
import Data.ByteString.Char8 ()

import AWS.Class
import AWS.Credential

initialEC2Context :: HTTP.Manager -> AWSContext
initialEC2Context mgr = AWSContext
    { manager = mgr
    , endpoint = "ec2.amazonaws.com"
    , lastRequestId = Nothing
    }

type EC2 m a = AWS AWSContext m a

runEC2 :: MonadIO m => Credential -> AWS AWSContext m a -> m a
runEC2 = runAWS initialEC2Context

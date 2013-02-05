{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.CloudWatch
    ( -- * CloudWatch Environment
      CloudWatch
    , runCloudWatch
    , runCloudWatch'
    , setRegion
    , apiVersion
      -- * Metric
    , module AWS.CloudWatch.Metric
    , module AWS.CloudWatch.Alarm
    ) where

import Data.Text (Text)
import Data.Conduit
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.State as State
import qualified Network.HTTP.Conduit as HTTP
import Data.Monoid ((<>))

import AWS.Class
import AWS.Lib.Query (textToBS)

import AWS
import AWS.CloudWatch.Internal
import AWS.CloudWatch.Metric
import AWS.CloudWatch.Alarm

initialCloudWatchContext :: HTTP.Manager -> AWSContext
initialCloudWatchContext mgr = AWSContext
    { manager = mgr
    , endpoint = "monitoring.amazonaws.com"
    , lastRequestId = Nothing
    }

runCloudWatch :: MonadIO m => Credential -> CloudWatch m a -> m a
runCloudWatch = runAWS initialCloudWatchContext

runCloudWatch' :: Monad m
    => HTTP.Manager -> Credential -> CloudWatch m a -> m a
runCloudWatch' mgr = runAWS' mgr initialCloudWatchContext

setRegion
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -> CloudWatch m ()
setRegion region = do
    ctx <- State.get
    State.put
        ctx { endpoint =
            "monitoring." <> textToBS region <> ".amazonaws.com"
            }

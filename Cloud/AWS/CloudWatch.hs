{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.CloudWatch
    ( -- * CloudWatch Environment
      CloudWatch
    , runCloudWatch
    , runCloudWatchwithManager
    , setRegion
    , apiVersion
      -- * Metric
    , module Cloud.AWS.CloudWatch.Metric
    , module Cloud.AWS.CloudWatch.Alarm
    ) where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.State as State
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import qualified Network.HTTP.Conduit as HTTP
import Data.Monoid ((<>))

import Cloud.AWS.Class
import Cloud.AWS.Lib.Query (textToBS)

import Cloud.AWS.CloudWatch.Internal
import Cloud.AWS.CloudWatch.Metric
import Cloud.AWS.CloudWatch.Alarm

initialCloudWatchContext :: HTTP.Manager -> AWSContext
initialCloudWatchContext mgr = AWSContext
    { manager = mgr
    , endpoint = "monitoring.amazonaws.com"
    , lastRequestId = Nothing
    }

runCloudWatch :: MonadIO m => CloudWatch m a -> m a
runCloudWatch = runAWS initialCloudWatchContext

runCloudWatchwithManager :: Monad m
    => HTTP.Manager -> AWSSettings -> CloudWatch m a -> m a
runCloudWatchwithManager mgr =
    runAWSwithManager mgr initialCloudWatchContext

setRegion
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -> CloudWatch m ()
setRegion region = do
    ctx <- State.get
    State.put
        ctx { endpoint =
            "monitoring." <> textToBS region <> ".amazonaws.com"
            }

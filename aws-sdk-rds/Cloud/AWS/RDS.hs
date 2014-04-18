{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.RDS
    ( -- * RDS Environment
      RDS
    , runRDS
    , runRDSwithManager
    , setRegion
      -- * DBInstance
    , module Cloud.AWS.RDS.DBInstance
      -- * DBParameterGroup
    , module Cloud.AWS.RDS.DBParameterGroup
      -- * DBSecurityGroup
    , module Cloud.AWS.RDS.DBSecurityGroup
      -- * DBSnapshot
    , module Cloud.AWS.RDS.DBSnapshot
      -- * DBSubnetGroup
    , module Cloud.AWS.RDS.DBSubnetGroup
      -- * Event
    , module Cloud.AWS.RDS.Event
      -- * EventSubscription
    , module Cloud.AWS.RDS.EventSubscription
      -- * OptionGroup
    , module Cloud.AWS.RDS.OptionGroup
      -- * Tag
    , module Cloud.AWS.RDS.Tag
    ) where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.State as State
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import qualified Network.HTTP.Conduit as HTTP
import Data.Monoid ((<>))

import Cloud.AWS.Class
import Cloud.AWS.Lib.Query (textToBS)

import Cloud.AWS.RDS.Internal
import Cloud.AWS.RDS.DBInstance
import Cloud.AWS.RDS.DBParameterGroup
import Cloud.AWS.RDS.DBSecurityGroup
import Cloud.AWS.RDS.DBSnapshot
import Cloud.AWS.RDS.DBSubnetGroup
import Cloud.AWS.RDS.Event
import Cloud.AWS.RDS.EventSubscription
import Cloud.AWS.RDS.OptionGroup
import Cloud.AWS.RDS.Tag

initialRDSContext :: HTTP.Manager -> AWSContext
initialRDSContext mgr = AWSContext
    { manager = mgr
    , endpoint = "rds.amazonaws.com"
    , lastRequestId = Nothing
    }

runRDS :: MonadIO m => RDS m a -> m a
runRDS = runAWS initialRDSContext

runRDSwithManager :: Monad m
    => HTTP.Manager -> AWSSettings -> RDS m a -> m a
runRDSwithManager mgr = runAWSwithManager mgr initialRDSContext

setRegion
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -> RDS m ()
setRegion region = do
    ctx <- State.get
    State.put
        ctx { endpoint =
            "rds." <> textToBS region <> ".amazonaws.com"
            }

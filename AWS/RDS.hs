{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.RDS
    ( -- * RDS Environment
      RDS
    , runRDS
    , runRDSwithManager
    , setRegion
      -- * DBInstance
    , module AWS.RDS.DBInstance
      -- * DBParameterGroup
    , module AWS.RDS.DBParameterGroup
      -- * DBSecurityGroup
    , module AWS.RDS.DBSecurityGroup
      -- * DBSnapshot
    , module AWS.RDS.DBSnapshot
      -- * DBSubnetGroup
    , module AWS.RDS.DBSubnetGroup
      -- * Event
    , module AWS.RDS.Event
      -- * EventSubscription
    , module AWS.RDS.EventSubscription
      -- * OptionGroup
    , module AWS.RDS.OptionGroup
    ) where

import Data.Text (Text)
import Data.Conduit
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.State as State
import qualified Network.HTTP.Conduit as HTTP
import Data.Monoid ((<>))

import AWS.Class
import AWS.Lib.Query (textToBS)

import AWS.RDS.Internal
import AWS.RDS.DBInstance
import AWS.RDS.DBParameterGroup
import AWS.RDS.DBSecurityGroup
import AWS.RDS.DBSnapshot
import AWS.RDS.DBSubnetGroup
import AWS.RDS.Event
import AWS.RDS.EventSubscription
import AWS.RDS.OptionGroup

initialRDSContext :: HTTP.Manager -> AWSContext
initialRDSContext mgr = AWSContext
    { manager = mgr
    , endpoint = "rds.amazonaws.com"
    , lastRequestId = Nothing
    }

runRDS :: MonadIO m => AWSSettings -> RDS m a -> m a
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

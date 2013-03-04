{-# LANGUAGE FlexibleContexts #-}

module AWSTests.RDSTests.Util
    ( testRDS
    , withDBInstance
    , waitUntilNotFound
    , withDBSnapshot
    , withEventSubscription
    , withDBParameterGroup
    )
    where

import qualified Control.Concurrent as CC
import qualified Control.Exception.Lifted as E
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Conduit (MonadBaseControl, MonadResource)
import Data.List (find)
import Data.Text (Text)
import Data.Conduit (ResourceT, runResourceT)

import AWS.RDS
import AWS.RDS.Types

testRDS
    :: Text
    -> RDS (ResourceT IO) a
    -> IO a
testRDS region request = do
    runResourceT $ runRDS $ do
        setRegion region
        request

withDBInstance
    :: (MonadBaseControl IO m, MonadResource m)
    => CreateDBInstanceRequest
    -> (DBInstance -> RDS m a)
    -> RDS m a
withDBInstance req = E.bracket
    (deleted >> createDBInstance req)
    (\dbi -> deleteDBInstance (dbInstanceIdentifier dbi) SkipFinalSnapshot)
  where
    describe = describeDBInstances Nothing Nothing Nothing
    f dbi = dbInstanceIdentifier dbi == createDBInstanceIdentifier req
    g dbi = dbInstanceStatus dbi == Just "available"
    delete dbi = deleteDBInstance (dbInstanceIdentifier dbi) SkipFinalSnapshot
    deleted = waitUntilNotFound describe f g delete

waitUntilNotFound
    :: (MonadIO m, Functor m, MonadBaseControl IO m, MonadResource m)
    => RDS m [a] -- describe resources
    -> (a -> Bool) -- is target resource
    -> (a -> Bool) -- is deletable resource
    -> (a -> RDS m b) -- delete resource
    -> RDS m ()
waitUntilNotFound describe match deletable delete = do
    rs <- describe
    case find match rs of
        Nothing -> return ()
        Just r
            | deletable r -> do
                delete r
                waitUntilNotFound describe match deletable delete
            | otherwise -> do
                liftIO $ CC.threadDelay 10000000
                waitUntilNotFound describe match deletable delete

withDBSnapshot
    :: (MonadBaseControl IO m, MonadResource m)
    => Text
    -> Text
    -> (DBSnapshot -> RDS m a)
    -> RDS m a
withDBSnapshot dbiid dbsid = E.bracket
    (createDBSnapshot dbiid dbsid)
    (deleteDBSnapshot . dbSnapshotIdentifier)

withEventSubscription
    :: (MonadBaseControl IO m, MonadResource m)
    => Text
    -> Text
    -> (EventSubscription -> RDS m a)
    -> RDS m a
withEventSubscription name arn = E.bracket
    (createEventSubscription Nothing [] arn [] Nothing name)
    (deleteEventSubscription . eventSubscriptionCustSubscriptionId)

withDBParameterGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text
    -> (DBParameterGroup -> RDS m a)
    -> RDS m a
withDBParameterGroup name = E.bracket
    (createDBParameterGroup "MySQL5.5" name "hspec-test")
    (deleteDBParameterGroup . dbParameterGroupName)

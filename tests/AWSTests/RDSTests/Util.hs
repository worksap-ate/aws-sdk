{-# LANGUAGE FlexibleContexts #-}

module AWSTests.RDSTests.Util
    ( testRDS
    , withDBInstance
    )
    where

import qualified Control.Concurrent as CC
import qualified Control.Exception.Lifted as E
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Conduit (MonadBaseControl, MonadResource)
import Data.List (find)
import Data.Text (Text)
import Data.Conduit (ResourceT, runResourceT)

import AWS
import AWS.RDS
import AWS.RDS.Types

testRDS
    :: Text
    -> RDS (ResourceT IO) a
    -> IO a
testRDS region request = do
    cred <- loadCredential
    runResourceT $ do
        runRDS cred $ do
            setRegion region
            request

withDBInstance
    :: (MonadBaseControl IO m, MonadResource m)
    => CreateDBInstanceRequest
    -> (DBInstance -> RDS m a)
    -> RDS m a
withDBInstance req = E.bracket
    (waitUntilNotFound f describe >> createDBInstance req)
    (\dbi -> deleteDBInstance (dbInstanceIdentifier dbi) SkipFinalSnapshot)
  where
    f dbi = dbInstanceIdentifier dbi == createDBInstanceIdentifier req
    describe = describeDBInstances Nothing Nothing Nothing

waitUntilNotFound
    :: (MonadIO m, Functor m)
    => (a -> Bool)
    -> RDS m [a]
    -> RDS m ()
waitUntilNotFound f describe = do
    rs <- describe
    case find f rs of
        Nothing -> return ()
        Just _ -> do
            liftIO $ CC.threadDelay 10000000
            waitUntilNotFound f describe

{-# LANGUAGE FlexibleContexts #-}

module AWSTests.RDSTests.Util
    ( testRDS
    , withDBInstance
    )
    where

import qualified Control.Exception.Lifted as E
import Data.Conduit
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
    (createDBInstance req)
    (\dbi -> deleteDBInstance (dbInstanceIdentifier dbi) SkipFinalSnapshot)

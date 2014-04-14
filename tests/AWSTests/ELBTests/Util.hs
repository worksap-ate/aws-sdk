{-# LANGUAGE CPP #-}

module AWSTests.ELBTests.Util
    ( testELB
    )
    where

import Data.Text (Text)
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
#else
import Data.Conduit (ResourceT, runResourceT)
#endif

import AWS
import AWS.ELB

testELB
    :: Text
    -> ELB (ResourceT IO) a
    -> IO a
testELB region request = do
    cred <- loadCredential
    runResourceT $ do
        runELB cred $ do
            setRegion region
            request

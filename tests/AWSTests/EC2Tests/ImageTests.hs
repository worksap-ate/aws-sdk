{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.ImageTests
    ( runImageTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runImageTests :: IO ()
runImageTests = hspec $ do
    describe "describeImages" $ do
        it "doesn't throw any exception" $ do
            testEC2 region (describeImages [] [] [] []) `miss` anyConnectionException

    describe "{create,deregister}Image" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withInstance testRunInstancesRequest $ \Instance{instanceId = inst} -> do
                    waitForInstanceState InstanceStateRunning inst
                    let name = "createImageTest"
                        desc = "For HSpec testing"
                    ami <- createImage inst name (Just desc) False []
                    deregisterImage ami
                ) `miss` anyConnectionException

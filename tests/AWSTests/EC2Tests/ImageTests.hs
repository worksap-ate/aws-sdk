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

    describe "{create,deregister}Image and {describe,modify}ImageAttribute" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withInstance testRunInstancesRequest $ \Instance{instanceId = inst} -> do
                    waitForInstanceState InstanceStateRunning inst
                    let name = "createImageTest"
                        desc = "For HSpec testing"
                    withImage inst name (Just desc) False [] $ \ami -> do
                        waitForImageState ImageStateAvailable ami
                        mapM_ (describeImageAttribute ami) allAttributes
                        let params =
                                [ LaunchPermissionItemGroup "all"
                                , LaunchPermissionItemUserId "111122223333"
                                , LaunchPermissionItemUserId "333322221111"
                                ]
                        modifyImageAttribute ami (Just $ LaunchPermission params []) [] Nothing
                        mapM_ (describeImageAttribute ami) allAttributes
                        modifyImageAttribute ami (Just $ LaunchPermission [] params) [] Nothing
                ) `miss` anyConnectionException

allAttributes :: [AMIAttribute]
allAttributes =
    [ AMIDescription
    , AMIKernel
    , AMIRamdisk
    , AMILaunchPermission
    , AMIProductCodes
    , AMIBlockDeviceMapping
    ]

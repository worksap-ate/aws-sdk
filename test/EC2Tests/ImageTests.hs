{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module EC2Tests.ImageTests
    ( runImageTests
    )
    where

import Control.Monad ((>=>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types
import Util
import EC2Tests.Util

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
                    snaps <- withImage inst name (Just desc) False [] $ \ami -> do
                        Image{imageBlockDeviceMappings = bdms} <- waitForImageState ImageStateAvailable ami
                        mapM_ (describeImageAttribute ami) allAttributes
                        let params =
                                [ LaunchPermissionItemGroup "all"
                                , LaunchPermissionItemUserId "111122223333"
                                , LaunchPermissionItemUserId "333322221111"
                                ]
                        modifyImageAttribute ami (Just $ LaunchPermission params []) [] Nothing
                        mapM_ (describeImageAttribute ami) allAttributes
                        modifyImageAttribute ami (Just $ LaunchPermission [] params) [] Nothing

                        return $ catMaybes $ map (blockDeviceMappingEbs >=> ebsSnapshotId) bdms
                    -- Cleanup snapshots created by createImage
                    mapM_ deleteSnapshot snaps
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

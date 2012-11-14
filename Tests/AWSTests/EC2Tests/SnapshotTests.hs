{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.SnapshotTests
    ( runSnapshotTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runSnapshotTests :: IO ()
runSnapshotTests = do
    hspec describeSnapshotsTest

describeSnapshotsTest :: Spec
describeSnapshotsTest = do
    describe "describeSnapshots doesn't fail" $ do
        it "describeSnapshots doesn't throw any exception" $ do
            testEC2 region (describeSnapshots [] [] [] []) `miss` anyHttpException

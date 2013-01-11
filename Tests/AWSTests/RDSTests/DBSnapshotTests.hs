module AWSTests.RDSTests.DBSnapshotTests
    ( runDBSnapshotTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.RDS
import AWSTests.Util
import AWSTests.RDSTests.Util

region :: Text
region = "ap-northeast-1"

runDBSnapshotTests :: IO ()
runDBSnapshotTests = do
    hspec describeDBSnapshotsTest

describeDBSnapshotsTest :: Spec
describeDBSnapshotsTest = do
    describe "describeDBSnapshots doesn't fail" $ do
        it "describeDBSnapshots doesn't throw any exception" $ do
            testRDS region (
                describeDBSnapshots Nothing Nothing Nothing Nothing Nothing
                ) `miss` anyHttpException

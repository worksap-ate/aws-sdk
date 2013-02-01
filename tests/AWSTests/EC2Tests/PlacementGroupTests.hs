module AWSTests.EC2Tests.PlacementGroupTests
    ( runPlacementGroupTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types (PlacementGroupStrategy(..))
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "us-east-1"

runPlacementGroupTests :: IO ()
runPlacementGroupTests = do
    hspec describePlacementGroupsTest
    hspec createAndDeletePlacementGroupTest

describePlacementGroupsTest :: Spec
describePlacementGroupsTest = do
    describe "describePlacementGroups doesn't fail" $ do
        it "describeInstances doesn't throw any exception" $ do
            testEC2 region (describePlacementGroups [] []) `miss` anyHttpException

createAndDeletePlacementGroupTest :: Spec
createAndDeletePlacementGroupTest = do
    describe "{create,delete}PlacementGroup doesn't fail" $ do
        it "{create,delete}PlacementGroup doesn't throw any exception" $ do
            testEC2' region (do
                createPlacementGroup groupName PlacementGroupStrategyCluster
                deletePlacementGroup groupName
                ) `miss` anyHttpException
  where
    groupName = "createAndDeletePlacementGroupTest"

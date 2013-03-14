module EC2Tests.PlacementGroupTests
    ( runPlacementGroupTests
    )
    where

import Data.Text (Text)
import Test.Hspec
import qualified Control.Exception.Lifted as E

import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types (PlacementGroupStrategy(..))
import Util
import EC2Tests.Util

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
            testEC2 region (describePlacementGroups [] []) `miss` anyConnectionException

createAndDeletePlacementGroupTest :: Spec
createAndDeletePlacementGroupTest = do
    describe "{create,delete}PlacementGroup doesn't fail" $ do
        it "{create,delete}PlacementGroup doesn't throw any exception" $ do
            testEC2' region (E.finally
                (createPlacementGroup groupName PlacementGroupStrategyCluster)
                (deletePlacementGroup groupName)
              ) `miss` anyConnectionException
  where
    groupName = "createAndDeletePlacementGroupTest"

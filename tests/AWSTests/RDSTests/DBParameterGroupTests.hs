module AWSTests.RDSTests.DBParameterGroupTests
    ( runDBParameterGroupTests
    )
    where

import Control.Applicative ((<$>))
import Data.Text (Text)
import Test.Hspec

import AWS.RDS
import AWS.RDS.Types
import AWSTests.Util
import AWSTests.RDSTests.Util

region :: Text
region = "ap-northeast-1"

runDBParameterGroupTests :: IO ()
runDBParameterGroupTests = hspec $ do
    describeDBParameterGroupsTest
    createAndDeleteDBParameterGroupTest
    describeDBParametersTest

describeDBParameterGroupsTest :: Spec
describeDBParameterGroupsTest = do
    describe "describeDBParameterGroups doesn't fail" $ do
        it "describeDBParameterGroups doesn't throw any exception" $ do
            testRDS region (describeDBParameterGroups Nothing Nothing Nothing)
                `miss` anyConnectionException

createAndDeleteDBParameterGroupTest :: Spec
createAndDeleteDBParameterGroupTest = do
    describe "{create,delete}DBParameterGroup doesn't fail" $ do
        it "{create,delete}DBParameterGroup doesn't throw any exception" $ do
            testRDS region (do
                withDBParameterGroup name $ const $ return ()
                ) `miss` anyConnectionException
  where
    name = "hspec-test-parameter-group"

describeDBParametersTest :: Spec
describeDBParametersTest = do
    describe "describeDBParameters doesn't fail" $ do
        it "describeDBParameters doesn't throw any exception" $ do
            testRDS region (do
                name <- dbParameterGroupName . head <$>
                     describeDBParameterGroups Nothing Nothing Nothing
                loop name Nothing
                ) `miss` anyConnectionException
  where
    loop name marker = do
        (marker', _) <- describeDBParameters name marker (Just 100) Nothing
        maybe (return ()) (\m -> loop name (Just m)) marker'

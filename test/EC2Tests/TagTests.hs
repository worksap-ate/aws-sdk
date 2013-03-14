{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module EC2Tests.TagTests
    ( runTagTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types
import qualified Cloud.AWS.EC2.Util as Util
import Util
import EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runTagTests :: IO ()
runTagTests = hspec $ do
    describeTagsTest
    createAndDeleteTagsTest

describeTagsTest :: Spec
describeTagsTest = do
    describe "describeTags" $ do
        it "doesn't throw any exception" $ do
            testEC2 region (describeTags []) `miss` anyConnectionException

createAndDeleteTagsTest :: Spec
createAndDeleteTagsTest = do
    describe "{create,delete}Tags" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (do
                Image{imageId = image}:_ <- Util.list $ describeImages [] [] [] []
                SecurityGroup{securityGroupId = sg}:_ <- Util.list $ describeSecurityGroups [] [] []
                let ids = [image, sg]
                    tags = [("createTagsTest", "tag1"), ("deleteTagsTest", "tag2")]
                    rtags = map (\(k,v) -> ResourceTag k (Just v)) tags
                createTags ids tags
                deleteTags ids rtags
                ) `miss` anyConnectionException

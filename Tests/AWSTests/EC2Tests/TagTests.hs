{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.TagTests
    ( runTagTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runTagTests :: IO ()
runTagTests = do
    hspec describeTagsTest

describeTagsTest :: Spec
describeTagsTest = do
    describe "describeTags doesn't fail" $ do
        it "describeTags doesn't throw any exception" $ do
            testEC2 region (describeTags []) `shouldntThrow` anyException

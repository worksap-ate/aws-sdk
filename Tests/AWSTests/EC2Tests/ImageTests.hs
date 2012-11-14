{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.ImageTests
    ( runImageTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runImageTests :: IO ()
runImageTests = do
    hspec describeImageTest

describeImageTest :: Spec
describeImageTest = do
    describe "describeImages doesn't fail" $ do
        it "describeImages doesn't throw any exception" $ do
            testEC2 region (describeImages [] [] [] []) `miss` anyHttpException

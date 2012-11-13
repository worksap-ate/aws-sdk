{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.AddressTests
    ( runAddressTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-southeast-1"

runAddressTests :: IO ()
runAddressTests = hspec $ do
    describe "Address APIs don't fail" $ do
        it "describeAddresses doesn't throw any exception" $ do
            testEC2 region (describeAddresses [] [] [])
                `shouldntThrow` anyException
        it "create and delete Addresses" $ do
            testEC2' region (do
                addr <- allocateAddress False
                releaseAddress (Just $ alaPublicIp addr) Nothing
                addr2 <- allocateAddress True
                releaseAddress Nothing (alaAllocationId addr2)
                ) `shouldntThrow` anyException
    -- attach/detach

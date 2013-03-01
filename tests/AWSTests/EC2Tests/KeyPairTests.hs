{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.KeyPairTests
    ( runKeyPairTests
    )
    where

import Crypto.PubKey.RSA (generate)
import Crypto.Random.API (getSystemRandomGen)
import Data.Certificate.KeyRSA (encodePublic)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Base64.Lazy (encode)
import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types (KeyPair(..))
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runKeyPairTests :: IO ()
runKeyPairTests = hspec $ do
    describe "describeKeyPairs doesn't fail" $ do
        it "describeKeyPairs doesn't throw any exception" $ do
            testEC2 region (describeKeyPairs [] []) `miss` anyConnectionException

    describe "{create,delete}KeyPair" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (do
                (keypair, _) <- createKeyPair "TestKeyPair"
                deleteKeyPair (keyPairName keypair)
                ) `miss` anyConnectionException

    describe "importKeyPair" $ do
        it "doesn't throw any exception" $ do
            gen <- getSystemRandomGen
            let ((pubkey, _), _) = generate gen 128 1024
                der = S.concat $ L.toChunks $ encode $ encodePublic pubkey
            testEC2' region (do
                keypair <- importKeyPair "importKeyPairTest" der
                deleteKeyPair $ keyPairName keypair
                ) `miss` anyConnectionException

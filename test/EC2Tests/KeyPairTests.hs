{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}

module EC2Tests.KeyPairTests
    ( runKeyPairTests
    )
    where

import Control.Applicative ((<$>))
import Crypto.PubKey.RSA (generate)
import Crypto.Random.API (cprgCreate)
import Crypto.Random (SystemRNG, createEntropyPool)
import Data.Certificate.KeyRSA (encodePublic)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Base64.Lazy (encode)
import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types (KeyPair(..))
import Util
import EC2Tests.Util

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
            (gen :: SystemRNG) <- cprgCreate <$> createEntropyPool
            let ((pubkey, _), _) = generate gen 256 65537
                der = S.concat $ L.toChunks $ encode $ encodePublic pubkey
            testEC2' region (do
                keypair <- importKeyPair "importKeyPairTest" der
                deleteKeyPair $ keyPairName keypair
                ) `miss` anyConnectionException

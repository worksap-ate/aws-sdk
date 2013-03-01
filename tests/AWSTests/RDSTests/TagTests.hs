module AWSTests.RDSTests.TagTests
    ( runRDSTagTests
    )
    where

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Data.Text (Text)
import Test.Hspec

import AWS.RDS
import AWS.RDS.Types
import AWSTests.Util
import AWSTests.RDSTests.Util

region :: Text
region = "ap-northeast-1"

runRDSTagTests :: IO ()
runRDSTagTests = hspec $ do
    listTagsForResourceTest

listTagsForResourceTest :: Spec
listTagsForResourceTest = do
    describe "listTagsForResource doesn't fail" $ do
        it "listTagsForResource doesn't throw any exception" $ do
            testRDS region (do
                dbiid <- dbInstanceIdentifier . head <$>
                    describeDBInstances Nothing Nothing Nothing
                listTagsForResource $ arn <> dbiid
                ) `miss` anyConnectionException
  where
    arn = "arn:aws:rds:" <> region <> ":049669284607:db:"

module RDSTests.TagTests
    ( runRDSTagTests
    )
    where

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.RDS
import Cloud.AWS.RDS.Types
import Util
import RDSTests.Util

region :: Text
region = "ap-northeast-1"

runRDSTagTests :: IO ()
runRDSTagTests = hspec $ do
    listTagsForResourceTest
    addAndRemoveTagsTest

listTagsForResourceTest :: Spec
listTagsForResourceTest = do
    describe "listTagsForResource doesn't fail" $ do
        it "listTagsForResource doesn't throw any exception" $ do
            testRDS region (do
                dbiid <- dbInstanceIdentifier . head <$>
                    describeDBInstances Nothing Nothing Nothing
                listTagsForResource $ arn dbiid
                ) `miss` anyConnectionException

arn :: Text -> Text
arn dbiid = "arn:aws:rds:" <> region <> ":049669284607:db:" <> dbiid

addAndRemoveTagsTest :: Spec
addAndRemoveTagsTest = do
    describe "{add,remove}Tags{To,From}Resouce doesn't fail" $ do
        it "{add,remove}Tags{To,From}Resouce doesn't throw any exception" $ do
            testRDS region (do
                dbiid <- dbInstanceIdentifier . head <$>
                    describeDBInstances Nothing Nothing Nothing
                let tags =
                        [ Tag { tagKey = "hspec-test1"
                              , tagValue =  "test1"
                              }
                        , Tag { tagKey = "hspec-test2"
                              , tagValue = "test2"
                              }
                        ]
                addTagsToResource (arn dbiid) tags
                removeTagsFromResource (arn dbiid) $ map tagKey tags
                ) `miss` anyConnectionException

module AWSTests.EC2Tests.NetworkInterfaceAttributeTests
    ( runNetworkInterfaceAttributeTests
    ) where

import Data.Text (Text)
import Data.Conduit (($$+-))
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (lift)
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types (NetworkInterface(..))
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runNetworkInterfaceAttributeTests :: IO ()
runNetworkInterfaceAttributeTests = do
    hspec resetNetworkInterfaceAttributeTest

resetNetworkInterfaceAttributeTest :: Spec
resetNetworkInterfaceAttributeTest = do
    describe "ResetNetworkInterfaceAttribute doesn't fail" $ do
        it "resetNetworkInterfaceSourceDestCheck doesn't throw any exception" $ do
            testEC2' region (do
                ifacesSrc <- describeNetworkInterfaces [] []
                Just iface <- lift $ ifacesSrc $$+- CL.head
                resetNetworkInterfaceSourceDestCheck $ networkInterfaceId iface
                ) `miss` anyHttpException

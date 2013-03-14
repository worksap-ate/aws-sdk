module EC2Tests.AclTests (runAclTests) where

import Data.List (find)
import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types
import qualified Cloud.AWS.EC2.Util as Util
import Util
import EC2Tests.Util

region :: Text
region = "ap-southeast-1"

runAclTests :: IO ()
runAclTests = hspec $ do
    describe "describeNetworkAcls" $ do
        it "doesn't throw any exception" $ do
            testEC2 region (describeNetworkAcls [] []) `miss` anyConnectionException

    describe "{create,delete}NetworkAcl" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (do
                withVpc "10.0.0.0/24" $ \Vpc{vpcId = vpc} ->
                    withNetworkAcl vpc $ const (return ())
                ) `miss` anyConnectionException

    describe "replaceNetworkAclAssociation" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (do
                withSubnet "10.0.0.0/24" $ \Subnet{subnetId = subnet, subnetVpcId = vpc} ->
                    withNetworkAcl vpc $ \NetworkAcl{networkAclId = acl} -> do
                        -- Find the association in the subnet
                        Just NetworkAcl{networkAclAssociationSet = assocs} <- Util.head $ describeNetworkAcls [] [("association.subnet-id", [subnet])]
                        let Just assoc = find ((== subnet) . networkAclAssociationSubnetId) assocs
                        -- Replace and restore
                        newAssoc <- replaceNetworkAclAssociation (networkAclAssociationId assoc) acl
                        replaceNetworkAclAssociation newAssoc $ networkAclAssociationNetworkAclId assoc
                ) `miss` anyConnectionException

    describe "{create,replace,delete}NetworkAclEntry" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (do
                withVpc "10.0.0.0/24" $ \Vpc{vpcId = vpc} ->
                    withNetworkAcl vpc $ \NetworkAcl{networkAclId = acl} -> do
                        let e = testEntry acl
                        withNetworkAclEntry e $ do
                            return ()
                            replaceNetworkAclEntry $ e
                                { networkAclEntryRequestProtocol = 17
                                , networkAclEntryRequestPortRange = Just (PortRange 1000 2000)
                                }
                ) `miss` anyConnectionException

testEntry :: Text -> NetworkAclEntryRequest
testEntry acl = NetworkAclEntryRequest
    { networkAclEntryRequestNetworkAclId = acl
    , networkAclEntryRequestRuleNumber = 110
    , networkAclEntryRequestProtocol = -1
    , networkAclEntryRequestRuleAction = NetworkAclRuleActionDeny
    , networkAclEntryRequestEgress = False
    , networkAclEntryRequestCidrBlock = "10.0.0.0/24"
    , networkAclEntryRequestIcmp = Nothing
    , networkAclEntryRequestPortRange = Nothing
    }

module Main where

import RDSTests.DBInstanceTests
import RDSTests.DBParameterGroupTests
import RDSTests.DBSecurityGroupTests
import RDSTests.DBSnapshotTests
import RDSTests.DBSubnetGroupTests
import RDSTests.EventTests
import RDSTests.EventSubscriptionTests
import RDSTests.OptionGroupTests
import RDSTests.TagTests

main :: IO ()
main = do
    runDBSnapshotTests
    runDBInstanceTests
    runDBParameterGroupTests
    runDBSecurityGroupTests
    runDBSubnetGroupTests
    runEventTests
    runEventSubscriptionTests
    runOptionGroupTests
    runRDSTagTests

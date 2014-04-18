module Cloud.AWS.RDS.Util
    ( wait
    , createDBInstanceRequest
    ) where

import Control.Applicative ((<$>))
import qualified Control.Concurrent as CC
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Safe

import Cloud.AWS.RDS
import Cloud.AWS.RDS.Types

wait
    :: (MonadIO m, Functor m)
    => (a -> Bool) -- ^ condition
    -> (Text -> RDS m [a]) -- ^ DescribeResources
    -> Text -- ^ Resource Id
    -> RDS m a
wait f g rid = do
    mr <- headMay <$> g rid
    case mr of
        Nothing -> fail $ "Resource not found: " ++ T.unpack rid
        Just r  -> if f r
            then return r
            else do
                liftIO $ CC.threadDelay 10000000
                wait f g rid

-- | copy from DBInstance
createDBInstanceRequest
    :: DBInstance
    -> Text -- ^ New DBIdentifier
    -> Text -- ^ MasterUserPassword
    -> CreateDBInstanceRequest
createDBInstanceRequest db newid passwd = CreateDBInstanceRequest
    (dbInstanceAllocatedStorage db)
    (Just $ dbInstanceAutoMinorVersionUpgrade db)
    (dbInstanceAvailabilityZone db)
    (Just $ dbInstanceBackupRetentionPeriod db)
    (dbInstanceCharacterSetName db)
    (dbInstanceClass db)
    newid
    (dbInstanceDBName db)
    (fmap dbParameterGroupStatusName $ headMay $ dbInstanceDBParameterGroups db)
    (map dbSecurityGroupMembershipName $ dbInstanceSecurityGroups db)
    (dbSubnetGroupName <$> dbInstanceSubnetGroup db)
    (dbInstanceEngine db)
    (Just $ dbInstanceEngineVersion db)
    (dbInstanceIops db)
    (Just $ dbInstanceLicenseModel db)
    passwd
    (dbInstanceMasterUsername db)
    (Just $ dbInstanceMultiAZ db)
    (optionGroupMembershipName <$> listToMaybe (dbInstanceOptionGroupMemberships db))
    (endpointPort <$> dbInstanceEndpoint db)
    (Just $ dbInstancePreferredBackupWindow db)
    (Just $ dbInstancePreferredMaintenanceWindow db)
    (Just $ dbInstancePubliclyAccessible db)
    (map vpcSecurityGroupId $ dbInstanceVpcSecurityGroups db)

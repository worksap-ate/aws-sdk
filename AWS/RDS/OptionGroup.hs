{-# LANGUAGE FlexibleContexts, CPP #-}

module AWS.RDS.OptionGroup
    ( describeOptionGroups
    , createOptionGroup
    , deleteOptionGroup
    ) where

import Control.Applicative ((<$>), (<*>))
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
#endif
import Data.Conduit
import Data.Text (Text)
import Data.XML.Types (Event)

import AWS.Lib.Parser
import AWS.Lib.Query
import AWS.RDS.Internal
import AWS.RDS.Types (OptionGroup(..), Option(..))
import AWS.Util (toText)

describeOptionGroups
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ EngineName
    -> Maybe Text -- ^ MajorEngineVersion
    -> Maybe Text -- ^ Marker
    -> Maybe Int -- ^ MaxRecords
    -> Maybe Text -- ^ OptionGroupName
    -> RDS m [OptionGroup]
describeOptionGroups engine ver marker maxRecords name =
    rdsQuery "DescribeOptionGroups" params $
        elements' "OptionGroupsList" "OptionGroup" optionGroupSink
  where
    params =
        [ "EngineName" |=? engine
        , "MajorEngineVersion" |=? ver
        , "Marker" |=? marker
        , "MaxRecords" |=? toText <$> maxRecords
        , "OptionGroupName" |=? name
        ]

optionGroupSink
    :: MonadThrow m
    => Consumer Event m OptionGroup
optionGroupSink = OptionGroup
    <$> getT "AllowsVpcAndNonVpcInstanceMemberships"
    <*> getT "MajorEngineVersion"
    <*> getT "OptionGroupName"
    <*> getT "VpcId"
    <*> getT "EngineName"
    <*> getT "OptionGroupDescription"
    <*> elements "Option" optionSink

optionSink
    :: MonadThrow m
    => Consumer Event m Option
optionSink = Option
    <$> getT "Port"
    <*> getT "OptionName"
    <*> getT "OptionDescription"
    <*> elements "VpcSecurityGroupMembership" vpcSecurityGroupMembershipSink
    <*> elements' "DBSecurityGroupMemberships" "DBSecurityGroup"
        dbSecurityGroupMembershipSink

createOptionGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ EngineName
    -> Text -- ^ MajorEngineVersion
    -> Text -- ^ OptionGroupDescription
    -> Text -- ^ OptionGroupName
    -> RDS m OptionGroup
createOptionGroup engine ver desc name =
    rdsQuery "CreateOptionGroup" params $
        element "OptionGroup" optionGroupSink
  where
    params =
        [ "EngineName" |= engine
        , "MajorEngineVersion" |= ver
        , "OptionGroupDescription" |= desc
        , "OptionGroupName" |= name
        ]

deleteOptionGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ OptionGroupName
    -> RDS m ()
deleteOptionGroup name =
    rdsQueryOnlyMetadata "DeleteOptionGroup"
        [ "OptionGroupName" |= name ]

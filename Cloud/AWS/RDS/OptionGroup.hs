{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.RDS.OptionGroup
    ( describeOptionGroups
    , createOptionGroup
    , deleteOptionGroup
    , describeOptionGroupOptions
    , modifyOptionGroup
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Conduit
import Data.Text (Text)
import Data.XML.Types (Event)

import Cloud.AWS.Lib.Parser
import Cloud.AWS.Lib.Query
import Cloud.AWS.RDS.Internal
import Cloud.AWS.RDS.Types.OptionGroup
import Cloud.AWS.Util (toText, boolToText)

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

describeOptionGroupOptions
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ EngineName
    -> Maybe Text -- ^ MajorEngineVersion
    -> Maybe Text -- ^ Marker
    -> Maybe Int -- ^ MaxRecords
    -> RDS m [OptionGroupOption]
describeOptionGroupOptions name version marker maxRecords =
    rdsQuery "DescribeOptionGroupOptions" params $
        elements "OptionGroupOption" optionGroupOptionSink
  where
    params =
        [ "EngineName" |= name
        , "MajorEngineVersion" |=? version
        , "Marker" |=? marker
        , "MaxRecords" |=? toText <$> maxRecords
        ]

optionGroupOptionSink
    :: MonadThrow m
    => Consumer Event m OptionGroupOption
optionGroupOptionSink = OptionGroupOption
    <$> getT "MajorEngineVersion"
    <*> getT "PortRequired"
    <*> elements' "OptionsDependedOn" "OptionName" text
    <*> getT "Description"
    <*> getT "DefaultPort"
    <*> getT "Name"
    <*> getT "EngineName"
    <*> getT "MinimumRequiredMinorEngineVersion"

modifyOptionGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ OptionGroupName
    -> ModifyOptionGroupRequest -- ^ OptionsToInclude or OptionsToRemove
    -> Maybe Bool -- ^ ApplyImmediately
    -> RDS m OptionGroup
modifyOptionGroup name req imm =
    rdsQuery "ModifyOptionGroup" params $
        element "OptionGroup" optionGroupSink
  where
    params =
        [ "OptionGroupName" |= name
        , "ApplyImmediately" |=? boolToText <$> imm
        , reqParam req
        ]
    reqParam (OptionsToInclude confs) =
        "OptionsToInclude.member" |.#. map confParams confs
    reqParam (OptionsToRemove names) =
        "OptionsToRemove.member" |.#= names
    confParams conf =
        [ "DBSecurityGroupMemberships.member" |.#=
            optionConfigurationDBSecurityGroupMemberships conf
        , "OptionName" |= optionConfigurationOptionName conf
        , "Port" |= toText (optionConfigurationPort conf)
        , "VpcSecurityGroupMemberships.member" |.#=
            optionConfigurationVpcSecurityGroupMemberships conf
        ]

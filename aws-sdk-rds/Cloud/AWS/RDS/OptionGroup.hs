{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.RDS.OptionGroup
    ( describeOptionGroups
    , createOptionGroup
    , deleteOptionGroup
    , describeOptionGroupOptions
    , modifyOptionGroup
    ) where

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<), element, content)
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
import Data.Text (Text)

import Cloud.AWS.Lib.Query
import Cloud.AWS.RDS.Internal
import Cloud.AWS.RDS.Types.OptionGroup

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
        , "MaxRecords" |=? maxRecords
        , "OptionGroupName" |=? name
        ]

optionGroupSink
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m OptionGroup
optionGroupSink xml = OptionGroup
    <$> xml .< "AllowsVpcAndNonVpcInstanceMemberships"
    <*> xml .< "MajorEngineVersion"
    <*> xml .< "OptionGroupName"
    <*> xml .< "VpcId"
    <*> xml .< "EngineName"
    <*> xml .< "OptionGroupDescription"
    <*> elements "Option" optionSink xml

optionSink
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m Option
optionSink xml = Option
    <$> xml .< "Port"
    <*> xml .< "OptionName"
    <*> xml .< "OptionDescription"
    <*> xml .< "Persistent"
    <*> elements "OptionSetting" optionSettingSink xml
    <*> elements "VpcSecurityGroupMembership" vpcSecurityGroupMembershipSink xml
    <*> elements' "DBSecurityGroupMemberships" "DBSecurityGroup"
        dbSecurityGroupMembershipSink xml

optionSettingSink
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m OptionSetting
optionSettingSink xml = OptionSetting
    <$> xml .< "AllowedValues"
    <*> xml .< "ApplyType"
    <*> xml .< "DataType"
    <*> xml .< "DefaultValue"
    <*> xml .< "Description"
    <*> xml .< "IsCollection"
    <*> xml .< "IsModifiable"
    <*> xml .< "Name"
    <*> xml .< "Value"

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
        , "MaxRecords" |=? maxRecords
        ]

optionGroupOptionSink
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m OptionGroupOption
optionGroupOptionSink xml = OptionGroupOption
    <$> xml .< "MajorEngineVersion"
    <*> xml .< "Persistent"
    <*> xml .< "PortRequired"
    <*> elements' "OptionsDependedOn" "OptionName" content xml
    <*> xml .< "Description"
    <*> xml .< "DefaultPort"
    <*> xml .< "Name"
    <*> elements "OptionGroupOptionSetting" optionGroupOptionSettingSink xml
    <*> xml .< "EngineName"
    <*> xml .< "MinimumRequiredMinorEngineVersion"

optionGroupOptionSettingSink
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m OptionGroupOptionSetting
optionGroupOptionSettingSink xml = OptionGroupOptionSetting
    <$> xml .< "AllowedValues"
    <*> xml .< "ApplyType"
    <*> xml .< "DefaultValue"
    <*> xml .< "IsModifiable"
    <*> xml .< "SettingDescription"
    <*> xml .< "SettingName"

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
        , "ApplyImmediately" |=? imm
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
        , "OptionSettings.member" |.#. settingParams <$>
            optionConfigurationOptionSettings conf
        , "Port" |=? optionConfigurationPort conf
        , "VpcSecurityGroupMemberships.member" |.#=
            optionConfigurationVpcSecurityGroupMemberships conf
        ]
    settingParams setting =
        [ "AllowedValues" |= optionSettingAllowedValues setting
        , "ApplyType" |= optionSettingApplyType setting
        , "DataType" |= optionSettingDataType setting
        , "DefaultValue" |= optionSettingDefaultValue setting
        , "Description" |= optionSettingDescription setting
        , "IsCollection" |= optionSettingIsCollection setting
        , "IsModifiable" |= optionSettingIsModifiable setting
        , "Name" |= optionSettingName setting
        , "Value" |= optionSettingValue setting
        ]

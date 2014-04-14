{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Cloud.AWS.RDS.DBParameterGroup
    ( describeDBParameterGroups
    , createDBParameterGroup
    , deleteDBParameterGroup
    , describeDBParameters
    , modifyDBParameterGroup
    , resetDBParameterGroup
    , describeDBEngineVersions
    , describeEngineDefaultParameters
    ) where

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<), element, elementM)
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
import Data.Text (Text)

import Cloud.AWS.Lib.Query ((|=), (|=?), (|.#.))
import Cloud.AWS.RDS.Internal (RDS, rdsQueryOnlyMetadata, rdsQuery, elements, elements')
import Cloud.AWS.RDS.Types hiding (Event)

describeDBParameterGroups
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ DBParameterGroupName
    -> Maybe Text -- ^ Marker
    -> Maybe Int -- ^ MaxRecords
    -> RDS m [DBParameterGroup]
describeDBParameterGroups name marker maxRecords =
    rdsQuery "DescribeDBParameterGroups" params $
        elements "DBParameterGroup" dbParameterGroupSink
  where
    params =
        [ "DBParameterGroupName" |=? name
        , "Marker" |=? marker
        , "MaxRecords" |=? maxRecords
        ]

dbParameterGroupSink
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m DBParameterGroup
dbParameterGroupSink xml = DBParameterGroup
    <$> xml .< "DBParameterGroupFamily"
    <*> xml .< "Description"
    <*> xml .< "DBParameterGroupName"

createDBParameterGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBParameterGroupFamily
    -> Text -- ^ DBParameterGroupName
    -> Text -- ^ Description
    -> RDS m DBParameterGroup
createDBParameterGroup family name desc =
    rdsQuery "CreateDBParameterGroup" params $
        element "DBParameterGroup" dbParameterGroupSink
  where
    params =
        [ "DBParameterGroupFamily" |= family
        , "DBParameterGroupName" |= name
        , "Description" |= desc
        ]

deleteDBParameterGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBParameterGroupName
    -> RDS m ()
deleteDBParameterGroup name =
    rdsQueryOnlyMetadata "DeleteDBParameterGroup"
        ["DBParameterGroupName" |= name]

describeDBParameters
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBParameterGroupName
    -> Maybe Text -- ^ Marker
    -> Maybe Int -- ^ MaxRecords
    -> Maybe Text -- ^ Source
    -> RDS m (Maybe Text, [Parameter]) -- ^ (Marker, Parameters)
describeDBParameters name marker maxRecords src =
    rdsQuery "DescribeDBParameters" params $ \xml -> (,)
        <$> xml .< "Marker"
        <*> elements "Parameter" parameterSink xml
  where
    params =
        [ "DBParameterGroupName" |= name
        , "Marker" |=? marker
        , "MaxRecords" |=? maxRecords
        , "Source" |=? src
        ]

parameterSink
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m Parameter
parameterSink xml = Parameter
    <$> xml .< "ParameterValue"
    <*> xml .< "DataType"
    <*> xml .< "Source"
    <*> xml .< "IsModifiable"
    <*> xml .< "Description"
    <*> xml .< "ApplyType"
    <*> xml .< "AllowedValues"
    <*> xml .< "ParameterName"
    <*> xml .< "MinimumEngineVersion"

modifyDBParameterGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBParameterGroupName
    -> [ModifyParameter] -- ^ Parameters
    -> RDS m Text
modifyDBParameterGroup name parameters =
    rdsQuery "ModifyDBParameterGroup" params (.< "DBParameterGroupName")
  where
    params =
        [ "DBParameterGroupName" |= name
        , "Parameters.member" |.#.
            map modifyParameterParams parameters
        ]
    modifyParameterParams ModifyParameter{..} =
        [ "ParameterName" |= modifyParameterName
        , "ParameterValue" |= modifyParameterValue
        , "ApplyMethod" |= modifyParameterApplyMethod
        ]

resetDBParameterGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text
    -> ResetParameterRequest
    -> RDS m Text
resetDBParameterGroup name req =
    rdsQuery "ResetDBParameterGroup" params (.< "DBParameterGroupName")
  where
    params =
        [ "DBParameterGroupName" |= name
        ] ++ reqParams req
    reqParams ResetAllParameters =
        [ "ResetAllParameters" |= True ]
    reqParams (ResetParameters parameters) =
        [ "Parameters.member" |.#.
            map resetParameterParams parameters
        , "ResetAllParameters" |= False
        ]
    resetParameterParams ResetParameter{..} =
        [ "ParameterName" |= resetParameterName
        , "ApplyMethod" |= resetParameterApplyMethod
        ]

describeDBEngineVersions
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ DBParameterGroupFamily
    -> Maybe Bool -- ^ DefaultOnly
    -> Maybe Text -- ^ Engine
    -> Maybe Text -- ^ EngineVersion
    -> Maybe Bool -- ^ ListSupportedCharacterSets
    -> Maybe Text -- ^ Marker
    -> Maybe Int -- ^ MaxRecords
    -> RDS m [DBEngineVersion]
describeDBEngineVersions family only engine ver list marker maxRec =
    rdsQuery "DescribeDBEngineVersions" params $
        elements "DBEngineVersion" dbEngineVersionSink
  where
    params =
        [ "DBParameterGroupFamily" |=? family
        , "DefaultOnly" |=? only
        , "Engine" |=? engine
        , "EngineVersion" |=? ver
        , "ListSupportedCharacterSets" |=? list
        , "Marker" |=? marker
        , "MaxRecords" |=? maxRec
        ]

dbEngineVersionSink
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m DBEngineVersion
dbEngineVersionSink xml = DBEngineVersion
    <$> xml .< "DBParameterGroupFamily"
    <*> xml .< "Engine"
    <*> elements' "SupportedCharacterSets" "CharacterSet" characterSetSink xml
    <*> xml .< "DBEngineDescription"
    <*> elementM "DefaultCharacterSet" characterSetSink xml
    <*> xml .< "EngineVersion"
    <*> xml .< "DBEngineVersionDescription"

characterSetSink
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m CharacterSet
characterSetSink xml = CharacterSet
    <$> xml .< "CharacterSetName"
    <*> xml .< "CharacterSetDescription"

describeEngineDefaultParameters
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBparameterGroupFamily
    -> Maybe Text -- ^ Marker
    -> Maybe Int -- ^ MaxRecords
    -> RDS m (Maybe Text, EngineDefaults) -- ^ (Marker, EngineDefaults)
describeEngineDefaultParameters family marker maxRecords =
    rdsQuery "DescribeEngineDefaultParameters" params $
        element "EngineDefaults" conv
  where
    params =
        [ "DBParameterGroupFamily" |= family
        , "Marker" |=? marker
        , "MaxRecords" |=? maxRecords
        ]
    conv xml' = (,)
        <$> xml' .< "Marker"
        <*> (\xml'' -> EngineDefaults
            <$> xml'' .< "DBParameterGroupFamily"
            <*> elements "Parameter" parameterSink xml''
            ) xml'

{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.KeyPair
    ( describeKeyPairs
    , createKeyPair
    , deleteKeyPair
    , importKeyPair
    ) where

import Data.Text (Text)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Types
import AWS.EC2.Class
import AWS.EC2.Query
import AWS.EC2.Parser
import AWS.Util

describeKeyPairs
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ PublicIps
    -> [Filter] -- ^ Filters
    -> EC2 m (Source m KeyPair)
describeKeyPairs names filters =
    ec2QuerySource "DescribeKeyPairs" params
        $ itemConduit "keySet" keyPairSink
  where
    params =
        [ ArrayParams "KeyName" names
        , FilterParams filters
        ]

keyPairSink :: MonadThrow m => GLSink Event m KeyPair
keyPairSink = KeyPair
    <$> getT "keyName"
    <*> getT "keyFingerprint"

createKeyPair
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ KeyName
    -> EC2 m (KeyPair, Text) -- ^ KeyPair and KeyMaterial
createKeyPair name =
    ec2Query "CreateKeyPair" [ValueParam "KeyName" name]
        $ (,) <$> keyPairSink <*> getT "keyMaterial"

deleteKeyPair
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ KeyName
    -> EC2 m Bool
deleteKeyPair name =
    ec2Query "DeleteKeyPair" [ValueParam "KeyName" name]
        $ getF "return" textToBool

importKeyPair
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ KeyName
    -> Text -- ^ PublicKeyMaterial
    -> EC2 m KeyPair
importKeyPair name material =
    ec2Query "ImportKeyPair" params keyPairSink
  where
    params =
        [ ValueParam "KeyName" name
        , ValueParam "PublicKeyMaterial" material
        ]

{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.EC2.KeyPair
    ( describeKeyPairs
    , createKeyPair
    , deleteKeyPair
    , importKeyPair
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as BASE
import Data.Text (Text)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Applicative

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query
import Cloud.AWS.Lib.Parser

describeKeyPairs
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ PublicIps
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m KeyPair)
describeKeyPairs names filters =
    ec2QuerySource "DescribeKeyPairs" params
        $ itemConduit "keySet" keyPairSink
  where
    params =
        [ "KeyName" |.#= names
        , filtersParam filters
        ]

keyPairSink :: MonadThrow m => Consumer Event m KeyPair
keyPairSink = KeyPair
    <$> getT "keyName"
    <*> getT "keyFingerprint"

createKeyPair
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ KeyName
    -> EC2 m (KeyPair, Text) -- ^ KeyPair and KeyMaterial
createKeyPair name =
    ec2Query "CreateKeyPair" ["KeyName" |= name]
        $ (,) <$> keyPairSink <*> getT "keyMaterial"

deleteKeyPair
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ KeyName
    -> EC2 m Bool
deleteKeyPair = ec2Delete "DeleteKeyPair" "KeyName"

importKeyPair
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ KeyName
    -> ByteString -- ^ PublicKeyMaterial
    -> EC2 m KeyPair
importKeyPair name material =
    ec2Query "ImportKeyPair" params keyPairSink
  where
    params =
        [ "KeyName" |= name
        , "PublicKeyMaterial" |= BASE.encode material
        ]

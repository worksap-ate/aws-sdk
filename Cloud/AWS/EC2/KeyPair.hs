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

import Data.Conduit
import Control.Applicative

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query
import Cloud.AWS.Lib.Parser.Unordered

describeKeyPairs
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ PublicIps
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m KeyPair)
describeKeyPairs names filters =
    ec2QuerySource "DescribeKeyPairs" params $
        xmlParserConduit "keySet" $ \xml ->
            getElement xml "item" keyPairConv
  where
    params =
        [ "KeyName" |.#= names
        , filtersParam filters
        ]

keyPairConv :: (MonadThrow m, Applicative m) => SimpleXML -> m KeyPair
keyPairConv xml = KeyPair
    <$> xml .< "keyName"
    <*> xml .< "keyFingerprint"

createKeyPair
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ KeyName
    -> EC2 m (KeyPair, Text) -- ^ KeyPair and KeyMaterial
createKeyPair name =
    ec2Query "CreateKeyPair" ["KeyName" |= name] $ xmlParser $ \xml ->
        (,) <$> keyPairConv xml <*> xml .< "keyMaterial"

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
    ec2Query "ImportKeyPair" params $ xmlParser keyPairConv
  where
    params =
        [ "KeyName" |= name
        , "PublicKeyMaterial" |= BASE.encode material
        ]

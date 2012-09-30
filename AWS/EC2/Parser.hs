module AWS.EC2.Parser
    ( module AWS.EC2.Parser.Internal
    , resourceTagSink
    , productCodeSink
    , stateReasonSink
    , volumeTypeSink
    ) where

import Control.Applicative
import Data.Conduit
import Data.XML.Types (Event)

import AWS.EC2.Types
import AWS.EC2.Parser.Internal
import AWS.Util

resourceTagSink :: MonadThrow m
    => GLSink Event m [ResourceTag]
resourceTagSink = itemsSet "tagSet" $
    ResourceTag
    <$> getT "key"
    <*> getMT "value"

productCodeSink :: MonadThrow m
    => GLSink Event m [ProductCode]
productCodeSink = itemsSet "productCodes" $
    ProductCode
    <$> getT "productCode"
    <*> getF "type" productCodeType

stateReasonSink :: MonadThrow m
    => GLSink Event m (Maybe StateReason)
stateReasonSink = elementM "stateReason" $
    StateReason
    <$> getT "code"
    <*> getT "message"

volumeTypeSink :: MonadThrow m
    => GLSink Event m VolumeType
volumeTypeSink = volumeType
    <$> getT "volumeType"
    <*> getM "iops" (textToInt <$>)

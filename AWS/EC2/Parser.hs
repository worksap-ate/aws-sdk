module AWS.EC2.Parser
    ( module AWS.EC2.Parser.Internal
    , resourceTagSink
    , productCodeSink
    , stateReasonSink
    ) where

import Control.Applicative
import Data.Conduit
import Data.XML.Types (Event)

import AWS.EC2.Types
import AWS.EC2.Parser.Internal

resourceTagSink :: MonadThrow m
    => GLSink Event m [ResourceTag]
resourceTagSink = itemsSet "tagSet" $
    resourceTag
    <$> getT "key"
    <*> getMT "value"

productCodeSink :: MonadThrow m
    => GLSink Event m [ProductCode]
productCodeSink = itemsSet "productCodes" $
    productCode
    <$> getT "productCode"
    <*> getF "type" productCodeType

stateReasonSink :: MonadThrow m
    => GLSink Event m (Maybe StateReason)
stateReasonSink = elementM "stateReason" $
    stateReason
    <$> getT "code"
    <*> getT "message"

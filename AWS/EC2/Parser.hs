{-# LANGUAGE OverloadedStrings #-}

module AWS.EC2.Parser
    ( module AWS.EC2.Parser.Internal
    , resourceTagSink
    , productCodeSink
    , stateReasonSink
    ) where

import Control.Applicative
import Data.Conduit
import Data.XML.Types

import AWS.EC2.Types
import AWS.EC2.Parser.Internal

resourceTagSink :: MonadThrow m
    => GLSink Event m [ResourceTag]
resourceTagSink = itemsSet "tagSet" $
    resourceTag
    <$> getT "key"
    <*> getT "value"

productCodeSink :: MonadThrow m
    => GLSink Event m [ProductCode]
productCodeSink = itemsSet "productCodes" $
    productCode
    <$> getT "productCode"
    <*> getF "type" t2productCodeType

stateReasonSink :: MonadThrow m
    => GLSink Event m (Maybe StateReason)
stateReasonSink = elementM "stateReason" $
    stateReason
    <$> getT "code"
    <*> getT "message"


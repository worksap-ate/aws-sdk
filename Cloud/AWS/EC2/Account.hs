{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.EC2.Account
    ( describeAccountAttributes
    ) where

import Control.Applicative ((<$>), (<*>), Applicative)
import Data.Conduit (MonadResource, MonadBaseControl, ResumableSource, MonadThrow)

import Cloud.AWS.EC2.Internal (EC2, itemConduit, itemsSet)
import Cloud.AWS.EC2.Query (ec2QuerySource)
import Cloud.AWS.EC2.Types.Account
import Cloud.AWS.Lib.Query ((|.#=))
import Cloud.AWS.Lib.Parser.Unordered (SimpleXML, (.<))

describeAccountAttributes
    :: (MonadResource m, MonadBaseControl IO m)
    => [AttributeName]
    -> EC2 m (ResumableSource m AccountAttribute)
describeAccountAttributes attrs =
    ec2QuerySource "DescribeAccountAttributes" params $
        itemConduit "accountAttributeSet" accountAttributeConv
  where
    params =
        [ "AttributeName" |.#= attrs
        ]

accountAttributeConv
    :: (MonadThrow m, Applicative m)
    => SimpleXML -> m AccountAttribute
accountAttributeConv xml = AccountAttribute
    <$> xml .< "attributeName"
    <*> itemsSet xml "attributeValueSet" (.< "attributeValue")

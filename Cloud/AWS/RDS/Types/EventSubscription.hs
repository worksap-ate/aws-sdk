{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.RDS.Types.EventSubscription
    ( EventSubscription(..)
    ) where

import Data.Text (Text)
import Cloud.AWS.Lib.FromText (deriveFromText)
import Cloud.AWS.RDS.Types.Event (SourceType)

data EventSubscription = EventSubscription
    { eventSubscriptionEnabled :: Bool
    , eventSubscriptionCustomerAwsId :: Text
    , eventSubscriptionSourceType :: Maybe SourceType
    , eventSubscriptionStatus :: EventSubscriptionStatus
    , eventSubscriptionSourceIds :: [Text]
    , eventSubscriptionCreationTime :: Text
    , eventSubscriptionEventCategories :: [Text]
    , eventSubscriptionCustSubscriptionId :: Text
    , eventSubscriptionSnsTopicArn :: Text
    }
  deriving (Show, Eq)

data EventSubscriptionStatus
    = EventSubscriptionStatusCreating
    | EventSubscriptionStatusModifying
    | EventSubscriptionStatusDeleting
    | EventSubscriptionStatusActive
    | EventSubscriptionStatusNoPermission
    | EventSubscriptionStatusTopicNotExist
  deriving (Show, Read, Eq)

deriveFromText "EventSubscriptionStatus"
    [ "creating"
    , "modifying"
    , "deleting"
    , "active"
    , "no-permission"
    , "topic-not-exist"
    ]

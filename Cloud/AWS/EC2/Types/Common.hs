{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.EC2.Types.Common
    ( Architecture(..)
    , Filter
    , EC2Return(..)
    , Group(..)
    , Hypervisor(..)
    , Placement(..)
    , Platform(..)
    , ProductCode(..)
    , ProductCodeType(..)
    , ResourceTag(..)
    , RootDeviceType(..)
    , ShutdownBehavior(..)
    , StateReason(..)
    , VirtualizationType(..)
    ) where

import Cloud.AWS.Lib.FromText (FromText(..), deriveFromText)
import Cloud.AWS.Lib.ToText (deriveToText)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

data Architecture
    = I386
    | X86_64
  deriving (Show, Read, Eq)

deriveToText "Architecture" [ "i386", "x86_64" ]

data EC2Return
    = EC2Success
    | EC2Error Text
  deriving (Show, Read, Eq)

instance FromText EC2Return where
    fromText t
        | t == "true" = return EC2Success
        | otherwise   = return $ EC2Error t

type Filter = (Text, [Text])

data Group = Group
    { groupId :: Maybe Text
    , groupName :: Text
    }
  deriving (Show, Read, Eq)

data Hypervisor
    = HypervisorOVM
    | HypervisorXen
  deriving (Show, Read, Eq)

deriveFromText "Hypervisor" ["ovm", "xen"]

data Placement = Placement
    { placementAvailabilityZone :: Text
    , placementGroupName :: Maybe Text
    , placementTenancy :: Maybe Text
    }
  deriving (Show, Read, Eq)

data Platform
    = PlatformWindows
    | PlatformOther
  deriving (Show, Read, Eq)

deriveToText "Platform"
    [ "Windows"
    , ""
    ]

instance FromText Platform where
    fromText t
        | t == "windows" = return PlatformWindows
        | otherwise      = return PlatformOther
    fromNamedText _ = return . fromMaybe PlatformOther . (>>= fromText)

data ProductCode = ProductCode
    { productCodeCode :: Text
    , productCodeType :: ProductCodeType
    }
  deriving (Show, Read, Eq)

data ProductCodeType
    = ProductCodeDevpay
    | ProductCodeMarketplace
  deriving (Show, Read, Eq)

data ResourceTag = ResourceTag
    { resourceTagKey :: Text
    , resourceTagValue :: Maybe Text
    }
  deriving (Show, Read, Eq)

data RootDeviceType
    = RootDeviceTypeEBS
    | RootDeviceTypeInstanceStore
  deriving (Show, Read, Eq)

data ShutdownBehavior
    = ShutdownBehaviorStop
    | ShutdownBehaviorTerminate
  deriving (Show, Read, Eq)

deriveToText "ShutdownBehavior"
    [ "stop"
    , "terminate"
    ]

data StateReason = StateReason
    { stateReasonCode :: Text
    , stateReasonMessage :: Text
    }
  deriving (Show, Read, Eq)

data VirtualizationType
    = VirtualizationTypeParavirtual
    | VirtualizationTypeHVM
  deriving (Show, Read, Eq)

deriveFromText "Architecture" ["i386", "x86_64"]
deriveFromText "ProductCodeType" ["devpay", "marketplace"]
deriveFromText "RootDeviceType" ["ebs", "instance-store"]
deriveFromText "ShutdownBehavior" ["stop", "terminate"]
deriveFromText "VirtualizationType" ["paravirtual", "hvm"]

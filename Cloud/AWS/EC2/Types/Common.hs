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

import Cloud.AWS.Lib.FromText
import Cloud.AWS.Lib.ToText
import Data.Maybe (fromMaybe)

data Architecture
    = I386
    | X86_64
  deriving (Show, Read, Eq)

instance ToText Architecture where
    toText I386 = "i386"
    toText X86_64 = "x86_64"

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
    { groupId :: Text
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
    , placementTenancy :: Text
    }
  deriving (Show, Read, Eq)

data Platform
    = PlatformWindows
    | PlatformOther
  deriving (Show, Read, Eq)

instance ToText Platform where
    toText PlatformWindows = "Windows"
    toText PlatformOther = error "unsupported opperation: Platform.toText"

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

instance ToText ShutdownBehavior where
    toText ShutdownBehaviorStop = "stop"
    toText ShutdownBehaviorTerminate = "terminate"

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

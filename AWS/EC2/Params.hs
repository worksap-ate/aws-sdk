module AWS.EC2.Params where

import Data.Text (Text)

import AWS.EC2.Types

data BlockDeviceMappingParam
    = BlockDeviceMappingParamEBS
        { bdmpEbsDeviceName :: Text
        , bdmpEbsNoDevice :: Maybe Bool
        , bdmpEbsSource :: EbsSource
        , bdmpEbsDeleteOnTermination :: Maybe Bool
        , bdmpEbsVolumeType :: Maybe VolumeType
        }
    | BlockDeviceMappingParamInstanceStore
        { bdmpIsDeviceName :: Text
        , bdmpIsNoDevice :: Maybe Bool
        , bdmpIsVirtualName :: Maybe Text
        }
  deriving (Show)

data EbsSource
    = EbsSnapshotId Text
    | EbsVolumeSize Int
  deriving (Show)

data NetworkInterfaceParam = NetworkInterfaceParam
    { nipInterfaceId :: Maybe Text
    , nipDeviceIndex :: Maybe Text
    , nipSubnetId :: Maybe Text
    , nipDescription :: Maybe Text
    , nipPrivateIpAddresses :: [Text]
    , nipSecurityGroupIds :: [Text]
    , nipDeleteOnTermination :: Maybe Bool
    }
  deriving (Show)

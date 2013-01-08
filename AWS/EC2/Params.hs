module AWS.EC2.Params
    ( blockDeviceMappingsParam
    , privateIpAddressesParam
    , volumeTypeParams
    ) where

import Control.Applicative
import Data.Text (Text)
import Data.IP (IPv4)

import AWS.EC2.Types
import AWS.Lib.Query
import AWS.Util

blockDeviceMappingsParam :: [BlockDeviceMappingParam] -> QueryParam
blockDeviceMappingsParam =
    ("BlockDeviceMapping" |.#.) . map blockDeviceMappingParams

blockDeviceMappingParams :: BlockDeviceMappingParam -> [QueryParam]
blockDeviceMappingParams (BlockDeviceMappingParamEbs dn nd s dot vt) =
    [ "DeviceName" |= dn
    , "NoDevice" |=? boolToText <$> nd
    , "Ebs" |. ebsSourceParams s ++
        ["DeleteOnTermination" |=? boolToText <$> dot] ++
        maybe [] volumeTypeParams vt
    ]
  where
    ebsSourceParams (EbsSourceSnapshotId sid) =
        ["SnapshotId" |= sid]
    ebsSourceParams (EbsSourceVolumeSize size) =
        ["VolumeSize" |= toText size]
blockDeviceMappingParams (BlockDeviceMappingParamInstanceStore dn nd vn) =
    [ "DeviceName"|= dn
    , "NoDevice" |=? boolToText <$> nd
    , "VirtualName" |=? vn
    ]

volumeTypeParams :: VolumeType -> [QueryParam]
volumeTypeParams VolumeTypeStandard =
    ["VolumeType" |= "standard"]
volumeTypeParams (VolumeTypeIO1 iops) =
    [ "VolumeType" |= "io1"
    , "Iops" |= toText iops
    ]

privateIpAddressesParam :: Text -> [IPv4] -> QueryParam
privateIpAddressesParam name =
    (name |.#.) . map (\a -> ["PrivateIpAddress" |= toText a])

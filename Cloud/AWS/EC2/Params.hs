module Cloud.AWS.EC2.Params
    ( blockDeviceMappingsParam
    , privateIpAddressesParam
    , volumeTypeParams
    ) where

import Data.Text (Text)
import Data.IP (IPv4)

import Cloud.AWS.EC2.Types
import Cloud.AWS.Lib.Query

blockDeviceMappingsParam :: [BlockDeviceMappingParam] -> QueryParam
blockDeviceMappingsParam =
    ("BlockDeviceMapping" |.#.) . map blockDeviceMappingParams

blockDeviceMappingParams :: BlockDeviceMappingParam -> [QueryParam]
blockDeviceMappingParams (BlockDeviceMappingParamEbs dn nd s dot vt) =
    [ "DeviceName" |= dn
    , "NoDevice" |=? nd
    , "Ebs" |. ebsSourceParams s ++
        ["DeleteOnTermination" |=? dot] ++
        maybe [] volumeTypeParams vt
    ]
  where
    ebsSourceParams (EbsSourceSnapshotId sid) =
        ["SnapshotId" |= sid]
    ebsSourceParams (EbsSourceVolumeSize size) =
        ["VolumeSize" |= size]
blockDeviceMappingParams (BlockDeviceMappingParamInstanceStore dn nd vn) =
    [ "DeviceName"|= dn
    , "NoDevice" |=? nd
    , "VirtualName" |=? vn
    ]

volumeTypeParams :: VolumeType -> [QueryParam]
volumeTypeParams VolumeTypeStandard =
    ["VolumeType" |= ("standard" :: Text)]
volumeTypeParams (VolumeTypeIO1 iops) =
    [ "VolumeType" |= ("io1" :: Text)
    , "Iops" |= iops
    ]

privateIpAddressesParam :: Text -> [IPv4] -> QueryParam
privateIpAddressesParam name =
    (name |.#.) . map (\a -> ["PrivateIpAddress" |= a])

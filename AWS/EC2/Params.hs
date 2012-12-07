module AWS.EC2.Params where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Data.IP (IPv4)

import AWS.EC2.Query (QueryParam(..))
import AWS.EC2.Types
import AWS.Util

blockDeviceMappingParams
    :: [BlockDeviceMappingParam] -> QueryParam
blockDeviceMappingParams =
    StructArrayParams "BlockDeviceMapping" . map kvs
  where
    kvs (BlockDeviceMappingParamEbs name dev src dot vtype) = 
        [ ("DeviceName", name)
        , ebsSource src
        ] ++ vtparam vtype ++ (uncurry f =<<
            [ ("NoDevice", boolToText <$> dev)
            , ("Ebs.DeleteOnTermination", boolToText <$> dot)
            ])
    kvs (BlockDeviceMappingParamInstanceStore name dev vname) =
        [("DeviceName", name)] ++ (uncurry f =<<
            [ ("NoDevice", boolToText <$> dev)
            , ("VirtualName", vname)
            ])

    ebsSource (EbsSourceSnapshotId sid) = ("Ebs.SnapshotId", sid)
    ebsSource (EbsSourceVolumeSize size) =
        ("Ebs.VolumeSize", T.pack $ show size)

    f n = maybe [] (\a -> [(n, a)])
    vtparam Nothing = []
    vtparam (Just VolumeTypeStandard) =
        [("Ebs.VolumeType", "standard")]
    vtparam (Just (VolumeTypeIO1 iops)) =
        [ ("Ebs.VolumeType", "io1")
        , ("Ebs.Iops", T.pack $ show iops)
        ]

privateIpAddressesParam :: Text -> [IPv4] -> QueryParam
privateIpAddressesParam name addrs = StructArrayParams
    name
    $ unconcat $ zip (repeat "PrivateIpAddress")
    $ map toText addrs

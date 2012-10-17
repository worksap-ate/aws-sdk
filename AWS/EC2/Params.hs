module AWS.EC2.Params where

import Control.Applicative
import qualified Data.Text as T

import AWS.EC2.Query (QueryParam(..))
import AWS.EC2.Types
import AWS.Util

blockDeviceMappingParams
    :: [BlockDeviceMappingParam] -> QueryParam
blockDeviceMappingParams =
    StructArrayParams "BlockDeviceMapping" . map kvs
  where
    kvs (BlockDeviceMappingParamEBS name dev src dot vtype) = 
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

    ebsSource (EbsSnapshotId sid) = ("Ebs.SnapshotId", sid)
    ebsSource (EbsVolumeSize size) =
        ("Ebs.VolumeSize", T.pack $ show size)

    f n = maybe [] (\a -> [(n, a)])
    vtparam Nothing = []
    vtparam (Just Standard) = [("Ebs.VolumeType", "standard")]
    vtparam (Just (IO1 iops)) =
        [ ("Ebs.VolumeType", "io1")
        , ("Ebs.Iops", T.pack $ show iops)
        ]

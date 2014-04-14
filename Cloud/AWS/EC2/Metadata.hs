-- | EC2 Meta-data service.
--   This is available to use within EC2/VPC instance.
module Cloud.AWS.EC2.Metadata
    ( latestVersion
      -- * Meta-data
    , amiId
    , amiLaunchIndex
    , amiManifestPath
    , blockDeviceMapping
    , hostname
    , instanceAction
    , instanceId
    , instanceType
    , kernelId
    , localHostname
    , localIpv4
    , mac
    , metrics
    , interfaces
    , availabilityZone
    , profile
    , publicKeys
    , reservationId
    , securityGroups
      -- * User-data
    , userdata
      -- * Instance identity
    , idPkcs7
    , idSignature
    , idDocument
    , query
	) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import Data.Conduit
import Network.HTTP.Conduit (HttpException)
import qualified Network.HTTP.Conduit as HTTP
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Monoid
import Control.Applicative
import Control.Exception

import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import Cloud.AWS.Lib.ToText (toText)

base :: String
base = "http://169.254.169.254"

version :: String
version = "/latest"

query :: String -> IO [ByteString]
query path = runResourceT $ do
    req <- liftIO $ HTTP.parseUrl (base <> path)
    mgr <- liftIO $ HTTP.newManager HTTP.conduitManagerSettings
    res <- HTTP.http req mgr
    HTTP.responseBody res $$+- CB.lines =$ CL.consume

latestVersion :: IO Text
latestVersion = toText . last . init <$> query ""

metadata :: String -> IO [ByteString]
metadata path = query $ version <> "/meta-data/" <> path

h :: Functor f => f [ByteString] -> f Text
h = fmap (toText . head)

t :: [ByteString] -> [Text]
t = map toText

ignore :: IO (Maybe a) -> IO (Maybe a)
ignore io = handle ignore' io
  where
    ignore' :: HttpException -> IO (Maybe a)
    ignore' _ = return Nothing

amiId :: IO Text
amiId = h $ metadata "ami-id"

amiLaunchIndex :: IO Text
amiLaunchIndex = h $ metadata "ami-launch-index"

amiManifestPath :: IO Text
amiManifestPath = h $ metadata "ami-manifest-path"

blockDeviceMapping :: IO [(Text, Text)]
blockDeviceMapping = do
    bs <- metadata bdm
    ds <- mapM (\b -> head <$> (metadata $ bdm <> BC.unpack b)) bs
    return $ zip (t bs) (t ds)
  where
    bdm = "block-device-mapping/"

hostname :: IO Text
hostname = h $ metadata "hostname"

instanceAction :: IO Text
instanceAction = h $ metadata "instance-action"

instanceId :: IO Text
instanceId = h $ metadata "instance-id"

instanceType :: IO Text
instanceType = h $ metadata "instance-type"

kernelId :: IO Text
kernelId = h $ metadata "kernel-id"

localHostname :: IO Text
localHostname = h $ metadata "local-hostname"

localIpv4 :: IO Text
localIpv4 = h $ metadata "local-ipv4"

mac :: IO Text
mac = h $ metadata "mac"

metrics :: IO Text
metrics = h $ metadata "metrics/vhostmd"

interfaces :: IO [(Text, [(Text, Text)])]
interfaces = do
    ms <- metadata macs
    vs <- mapM (\b -> (val b)) ms
    ts <- mapM (uncurry f) (zip ms vs)
    return $ zip (map (toText . BC.init) ms) ts
  where
    macs = "network/interfaces/macs/"
    f m ks = zip (t ks) <$> mapM (kv m) ks
    kv m k = toText . head <$> val (m <> k)
    val k = metadata $ macs <> BC.unpack k

availabilityZone :: IO Text
availabilityZone = h $ metadata "placement/availability-zone"

profile :: IO Text
profile = h $ metadata "profile"

publicKeys :: IO [Text]
publicKeys = t <$> metadata "public-keys"

reservationId :: IO Text
reservationId = h $ metadata "reservation-id"

securityGroups :: IO [Text]
securityGroups = t <$> metadata "security-groups"

userdata :: IO (Maybe Text)
userdata = ignore $
    Just <$> (h <$> query $ version <> "/user-data")

queryRaw :: String -> IO ByteString
queryRaw path = runResourceT $ do
    req <- liftIO $ HTTP.parseUrl (base <> path)
    mgr <- liftIO $ HTTP.newManager HTTP.conduitManagerSettings
    res <- HTTP.http req mgr
    mconcat <$> (HTTP.responseBody res $$+- CL.consume)

identity :: String -> IO Text
identity name =
    toText <$> (queryRaw $ is <> name)
  where
    is = version <> "/dynamic/instance-identity/"

idPkcs7 :: IO Text
idPkcs7 = identity "pkcs7"

idSignature :: IO Text
idSignature = identity "signature"

idDocument :: IO Text
idDocument = identity "document"

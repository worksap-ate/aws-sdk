module Cloud.AWS.EC2.Types.Route
    ( CreateRouteRequest(..)
    ) where

import Data.IP (AddrRange, IPv4)
import Data.Text (Text)

data CreateRouteRequest
    = CreateRouteToGateway
        { createRouteTableId :: Text
        , createRouteDestinationCidrBlock :: AddrRange IPv4
        , createRouteGatewayId :: Text
        }
    | CreateRouteToInstance
        { createRouteTableId :: Text
        , createRouteDestinationCidrBlock :: AddrRange IPv4
        , createRouteInstanceId :: Text
        }
    | CreateRouteToNetworkInterface
        { createRouteTableId :: Text
        , createRouteDestinationCidrBlock :: AddrRange IPv4
        , createRouteNetworkInterfaceId :: Text
        }
  deriving (Show, Read, Eq)

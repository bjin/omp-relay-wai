module Network.OmpRelayWai
  ( RelayState
  , app
  , newRelayState
  ) where

import Network.Wai                    (Application, rawPathInfo, rawQueryString)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets             qualified as WS

import Network.OmpRelayWai.Relay
import Network.OmpRelayWai.Static

app :: FilePath -> RelayState -> Application
app distDir state = websocketsOr WS.defaultConnectionOptions (relayServerApp state) httpFallback
  where
    httpFallback request respond =
        case parseRelayRequest (rawPathInfo request) (rawQueryString request) of
            Just _  -> relayHttpFallback request respond
            Nothing -> staticDistApp distDir request respond

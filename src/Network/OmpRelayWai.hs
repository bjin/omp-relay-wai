-- SPDX-License-Identifier: Apache-2.0
--
-- Copyright (C) 2026 Bin Jin. All Rights Reserved.

module Network.OmpRelayWai
  ( RelayConfig(..)
  , RelayState
  , app
  , defaultRelayConfig
  , newRelayState
  , newRelayStateWith
  ) where

import Network.Wai                    (Application, rawPathInfo, rawQueryString)
import Network.Wai.Handler.WebSockets (websocketsOr)

import Network.OmpRelayWai.Relay
import Network.OmpRelayWai.Static

-- | Compose the WebSocket relay and static file WAI application.
app :: RelayState -> Application
app state = websocketsOr (relayConnectionOptions state) (relayServerApp state) httpFallback
  where
    httpFallback request respond =
        case parseRelayRequest (rawPathInfo request) (rawQueryString request) of
            Just _  -> relayHttpFallback request respond
            Nothing -> staticDistApp request respond

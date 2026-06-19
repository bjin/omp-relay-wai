-- SPDX-License-Identifier: Apache-2.0
--
-- Copyright (C) 2026 Bin Jin. All Rights Reserved.

module Main
  ( main
  ) where

import Control.Monad (when)
import Network.HProx (getConfig, run)
import System.IO     (hPutStrLn, stderr)

import Network.OmpRelayWai             (app, newRelayState)
import Network.OmpRelayWai.HProxConfig (HProxConfigSanitization(..), sanitizeHproxConfig)
import Paths_omp_relay_wai             (getDataFileName)

-- | Run the hprox-compatible relay executable.
main :: IO ()
main = do
    config <- getConfig
    let (sanitizedConfig, HProxConfigSanitization{..}) = sanitizeHproxConfig config
    when ignoredWs $
        hPutStrLn stderr "omp-relay-wai: ignoring --ws because collab WebSocket relay is served locally at /r/<roomId>"
    when ignoredCatchAllRev $
        hPutStrLn stderr "omp-relay-wai: ignoring catch-all --rev because static web site is served locally"
    distDir <- getDataFileName "dist"
    state <- newRelayState
    run (app distDir state) sanitizedConfig

-- SPDX-License-Identifier: Apache-2.0
--
-- Copyright (C) 2026 Bin Jin. All Rights Reserved.

module Main
  ( main
  ) where

import Test.Hspec

import Network.OmpRelayWai.EnvelopeSpec    qualified as EnvelopeSpec
import Network.OmpRelayWai.HProxConfigSpec qualified as HProxConfigSpec
import Network.OmpRelayWai.RelaySpec       qualified as RelaySpec
import Network.OmpRelayWai.StaticSpec      qualified as StaticSpec

-- | Test suite entry point.
main :: IO ()
main = hspec $ do
    EnvelopeSpec.spec
    HProxConfigSpec.spec
    RelaySpec.spec
    StaticSpec.spec

-- SPDX-License-Identifier: Apache-2.0
--
-- Copyright (C) 2026 Bin Jin. All Rights Reserved.

module Network.OmpRelayWai.HProxConfigSpec
  ( spec
  ) where

import Network.HProx (Config(..), defaultConfig)

import Test.Hspec

import Network.OmpRelayWai.HProxConfig

-- | hprox configuration sanitization behavior.
spec :: Spec
spec = describe "Network.OmpRelayWai.HProxConfig" $ do
    it "clears configured websocket redirect and reports it" $ do
        let (sanitized, HProxConfigSanitization{..}) =
                sanitizeHproxConfig defaultConfig { _ws = Just "127.0.0.1:8080" }
        _ws sanitized `shouldBe` Nothing
        ignoredWs `shouldBe` True
        ignoredCatchAllRev `shouldBe` False

    it "leaves absent websocket redirect absent" $ do
        let (sanitized, HProxConfigSanitization{..}) =
                sanitizeHproxConfig defaultConfig { _ws = Nothing }
        _ws sanitized `shouldBe` Nothing
        ignoredWs `shouldBe` False
        ignoredCatchAllRev `shouldBe` False

    it "removes catch-all reverse proxy routes and reports them" $ do
        let catchAll     = (Nothing, "/", "127.0.0.1:8080")
            prefixed     = (Nothing, "/api/", "127.0.0.1:8081")
            domainScoped = (Just "example.com", "/", "127.0.0.1:8082")

            (sanitized, HProxConfigSanitization{..}) =
                sanitizeHproxConfig defaultConfig { _rev = [catchAll, prefixed, domainScoped] }
        _rev sanitized `shouldBe` [prefixed, domainScoped]
        ignoredWs `shouldBe` False
        ignoredCatchAllRev `shouldBe` True

    it "treats normalized root reverse proxy prefixes as catch-all routes" $ do
        let catchAll = (Nothing, "///", "127.0.0.1:8080")

            (sanitized, HProxConfigSanitization{..}) =
                sanitizeHproxConfig defaultConfig { _rev = [catchAll] }
        _rev sanitized `shouldBe` []
        ignoredCatchAllRev `shouldBe` True

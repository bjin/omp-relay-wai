-- SPDX-License-Identifier: Apache-2.0
--
-- Copyright (C) 2026 Bin Jin. All Rights Reserved.

module Network.OmpRelayWai.EnvelopeSpec
  ( spec
  ) where

import Data.ByteString qualified as BS

import Test.Hspec

import Network.OmpRelayWai.Envelope

-- | Envelope parser and renderer behavior.
spec :: Spec
spec = describe "Network.OmpRelayWai.Envelope" $ do
    it "round-trips rendered peer id and payload" $ do
        let payload = BS.pack [10, 20, 30]
        parseEnvelope (renderEnvelope (PeerId 7) payload) `shouldBe`
            Just Envelope
                { envelopePeerId  = PeerId 7
                , envelopePayload = payload
                }

    it "rewrites only the peer id header" $ do
        let original = BS.pack [0, 0, 0, 7, 99, 100]
        rewriteEnvelopePeer (PeerId 42) original `shouldBe`
            Just (BS.pack [0, 0, 0, 42, 99, 100])

    it "rejects inputs shorter than the header" $ do
        let short = BS.pack [1, 2, 3]
        parseEnvelope short `shouldBe` Nothing
        rewriteEnvelopePeer (PeerId 42) short `shouldBe` Nothing

    it "uses big-endian peer id encoding" $
        BS.take envelopeHeaderLength (renderEnvelope (PeerId 0x01020304) "payload") `shouldBe`
            BS.pack [0x01, 0x02, 0x03, 0x04]

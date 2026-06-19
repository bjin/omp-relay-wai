module Main
  ( main
  ) where

import Network.OmpRelayWai.EnvelopeSpec    qualified as EnvelopeSpec
import Network.OmpRelayWai.HProxConfigSpec qualified as HProxConfigSpec
import Network.OmpRelayWai.RelaySpec       qualified as RelaySpec
import Network.OmpRelayWai.StaticSpec      qualified as StaticSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
    EnvelopeSpec.spec
    HProxConfigSpec.spec
    RelaySpec.spec
    StaticSpec.spec

-- SPDX-License-Identifier: Apache-2.0
--
-- Copyright (C) 2026 Bin Jin. All Rights Reserved.

module Network.OmpRelayWai.HProxConfig
  ( HProxConfigSanitization(..)
  , sanitizeHproxConfig
  ) where

import Data.ByteString.Char8 qualified as BS8
import Network.HProx         (Config(..))

-- | Flags describing hprox options ignored by this wrapper.
data HProxConfigSanitization = HProxConfigSanitization
    { ignoredWs          :: !Bool
    , ignoredCatchAllRev :: !Bool
    }
  deriving (Eq, Show)

-- | Remove hprox routes that are served directly by this application.
sanitizeHproxConfig :: Config -> (Config, HProxConfigSanitization)
sanitizeHproxConfig config@Config{..} = (sanitizedConfig, sanitization)
  where
    sanitizedConfig = config
        { _ws  = Nothing
        , _rev = filteredRev
        }

    sanitization = HProxConfigSanitization
        { ignoredWs          = maybe False (const True) _ws
        , ignoredCatchAllRev = catchAllWasConfigured
        }

    filteredRev = filter (not . isCatchAllReverseRoute) _rev

    catchAllWasConfigured = length filteredRev /= length _rev

isCatchAllReverseRoute :: (Maybe BS8.ByteString, BS8.ByteString, BS8.ByteString) -> Bool
isCatchAllReverseRoute (Nothing, prefix, _) = normalizeReversePrefix prefix == "/"
isCatchAllReverseRoute (Just _, _, _)       = False

normalizeReversePrefix :: BS8.ByteString -> BS8.ByteString
normalizeReversePrefix prefix
    | BS8.null prefix = "/"
    | otherwise = stripTrailingSlash prefixed
  where
    prefixed
        | "/" `BS8.isPrefixOf` prefix = prefix
        | otherwise = "/" <> prefix

stripTrailingSlash :: BS8.ByteString -> BS8.ByteString
stripTrailingSlash value
    | BS8.length value > 1 && "/" `BS8.isSuffixOf` value =
        stripTrailingSlash $ BS8.init value
    | otherwise = value

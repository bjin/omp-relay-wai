-- SPDX-License-Identifier: Apache-2.0
--
-- Copyright (C) 2026 Bin Jin. All Rights Reserved.
{-# LANGUAGE TemplateHaskell #-}

module Network.OmpRelayWai.Static
  ( healthzResponse
  , methodNotAllowedResponse
  , notFoundResponse
  , staticDistApp
  , staticFilesApp
  ) where

import Data.ByteString                qualified as BS
import Data.FileEmbed                 (embedDir, makeRelativeToProject)
import Data.Text                      qualified as Text
import Network.HTTP.Types             (methodGet, methodHead, status200, status404, status405)
import Network.Wai
    (Application, Request, Response, pathInfo, requestMethod, responseLBS)
import Network.Wai.Application.Static
    (StaticSettings, embeddedSettings, ss404Handler, ssIndices, ssListing, ssLookupFile, staticApp)
import WaiAppStatic.Types             (LookupResult(..), unsafeToPiece)

embeddedDistFiles :: [(FilePath, BS.ByteString)]
embeddedDistFiles = $(makeRelativeToProject "dist" >>= embedDir)

-- | Serve health checks and embedded files from the generated distribution assets.
staticDistApp :: Application
staticDistApp = staticFilesApp embeddedDistFiles

-- | Serve health checks and static files from an embedded file list.
staticFilesApp :: [(FilePath, BS.ByteString)] -> Application
staticFilesApp files = serve
  where
    fileApp = staticApp $ embeddedStaticSettings files

    serve request respond
        | requestMethod request /= methodGet && requestMethod request /= methodHead =
            respond methodNotAllowedResponse
        | pathInfo request == ["healthz"] = respond $ healthzResponseFor request
        | not $ safePathInfo $ pathInfo request = respond notFoundResponse
        | otherwise = fileApp request respond

-- | Successful health check response body.
healthzResponse :: Response
healthzResponse = responseLBS status200 [("Content-Type", "text/plain")] "okay"

-- | Plain 404 response for missing or unsafe paths.
notFoundResponse :: Response
notFoundResponse = responseLBS status404 [] "not found"

-- | Response for methods not served by the static application.
methodNotAllowedResponse :: Response
methodNotAllowedResponse = responseLBS status405 [("Allow", "GET, HEAD")] ""

healthzResponseFor :: Request -> Response
healthzResponseFor request
    | requestMethod request == methodHead =
        responseLBS status200 [("Content-Type", "text/plain")] ""
    | otherwise = healthzResponse

embeddedStaticSettings :: [(FilePath, BS.ByteString)] -> StaticSettings
embeddedStaticSettings files =
    baseSettings
        { ss404Handler = Just notFoundApp
        , ssIndices    = [unsafeToPiece "index.html"]
        , ssListing    = Nothing
        , ssLookupFile = lookupFile
        }
  where
    baseSettings = embeddedSettings files

    lookupFile pieces = do
        result <- ssLookupFile baseSettings pieces
        return $ case result of
            LRFolder _ -> LRNotFound
            _          -> result

notFoundApp :: Application
notFoundApp _ respond = respond notFoundResponse

safePathInfo :: [Text.Text] -> Bool
safePathInfo = all isSafeSegment

isSafeSegment :: Text.Text -> Bool
isSafeSegment segment =
    not (Text.null segment)
        && not (Text.isPrefixOf "." segment)
        && not (Text.any isUnsafeSegmentChar segment)

isUnsafeSegmentChar :: Char -> Bool
isUnsafeSegmentChar char = char == '/' || char == '\\'


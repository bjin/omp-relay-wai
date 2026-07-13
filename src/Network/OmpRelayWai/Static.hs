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

import Control.Applicative   ((<|>))
import Data.ByteString       qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy  qualified as LBS
import Data.FileEmbed        (embedDir, makeRelativeToProject)
import Data.Map.Strict       qualified as Map
import Data.Text             qualified as Text
import Network.HTTP.Types
    (ResponseHeaders, methodGet, methodHead, status200, status404, status405)
import Network.Mime          (defaultMimeLookup)
import Network.Wai           (Application, Request, Response, pathInfo, requestMethod, responseLBS)

embeddedDistFiles :: [(FilePath, BS.ByteString)]
embeddedDistFiles = $(makeRelativeToProject "webui" >>= embedDir)

data EmbeddedFile = EmbeddedFile
    { embeddedFileContent :: !LBS.ByteString
    , embeddedFileHeaders :: !ResponseHeaders
    }

-- | Serve health checks and embedded files from the generated distribution assets.
staticDistApp :: Application
staticDistApp = staticFilesApp embeddedDistFiles

-- | Serve health checks and static files from an embedded file list.
staticFilesApp :: [(FilePath, BS.ByteString)] -> Application
staticFilesApp files = serve
  where
    fileMap = embeddedFileMap files

    serve request respond
        | requestMethod request /= methodGet && requestMethod request /= methodHead =
            respond methodNotAllowedResponse
        | pathInfo request == ["healthz"] = respond $ healthzResponseFor request
        | not $ safePathInfo $ pathInfo request = respond notFoundResponse
        | otherwise = respond $ embeddedFileResponse fileMap request

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

embeddedFileMap :: [(FilePath, BS.ByteString)] -> Map.Map [Text.Text] EmbeddedFile
embeddedFileMap =
    Map.fromList . map (\(filePath, content) -> (filePathPieces filePath, embeddedFile filePath content))

embeddedFile :: FilePath -> BS.ByteString -> EmbeddedFile
embeddedFile filePath content = EmbeddedFile
    { embeddedFileContent = LBS.fromChunks $ strictChunks embeddedChunkSize content
    , embeddedFileHeaders =
        [ ("Content-Type", defaultMimeLookup $ Text.pack filePath)
        , ("Content-Length", BS8.pack $ show $ BS.length content)
        , ("Access-Control-Allow-Origin", "*")
        ]
    }

embeddedFileResponse :: Map.Map [Text.Text] EmbeddedFile -> Request -> Response
embeddedFileResponse files request =
    case lookupEmbeddedFile files $ pathInfo request of
        Nothing   -> notFoundResponse
        Just file -> embeddedFileToResponse request file

lookupEmbeddedFile :: Map.Map [Text.Text] EmbeddedFile -> [Text.Text] -> Maybe EmbeddedFile
lookupEmbeddedFile files [] = Map.lookup ["index.html"] files
lookupEmbeddedFile files pieces =
    Map.lookup pieces files <|> Map.lookup (pieces <> ["index.html"]) files

embeddedFileToResponse :: Request -> EmbeddedFile -> Response
embeddedFileToResponse request EmbeddedFile{..} =
    responseLBS status200 embeddedFileHeaders body
  where
    body
        | requestMethod request == methodHead = ""
        | otherwise = embeddedFileContent

filePathPieces :: FilePath -> [Text.Text]
filePathPieces = map Text.pack . filter (not . null) . splitPathPieces

splitPathPieces :: FilePath -> [FilePath]
splitPathPieces "" = []
splitPathPieces path =
    piece : case rest of
        ""              -> []
        _separator:more -> splitPathPieces more
  where
    (piece, rest) = break isUnsafeSegmentChar path

embeddedChunkSize :: Int
embeddedChunkSize = 4096

strictChunks :: Int -> BS.ByteString -> [BS.ByteString]
strictChunks chunkSize content
    | BS.null content = []
    | otherwise = chunk : strictChunks chunkSize rest
  where
    (chunk, rest) = BS.splitAt chunkSize content

safePathInfo :: [Text.Text] -> Bool
safePathInfo = all isSafeSegment

isSafeSegment :: Text.Text -> Bool
isSafeSegment segment =
    not (Text.null segment)
        && not (Text.isPrefixOf "." segment)
        && not (Text.any isUnsafeSegmentChar segment)

isUnsafeSegmentChar :: Char -> Bool
isUnsafeSegmentChar char = char == '/' || char == '\\'


module Network.OmpRelayWai.Static
  ( healthzResponse
  , methodNotAllowedResponse
  , notFoundResponse
  , staticDistApp
  ) where

import Data.Text          qualified as Text
import Network.HTTP.Types (methodGet, methodHead, status200, status404, status405)
import Network.Mime       (defaultMimeLookup)
import Network.Wai
    (Application, Request, Response, pathInfo, requestMethod, responseFile, responseLBS)
import System.Directory   (doesFileExist)
import System.FilePath    ((</>))

staticDistApp :: FilePath -> Application
staticDistApp distDir request respond
  | requestMethod request /= methodGet && requestMethod request /= methodHead =
        respond methodNotAllowedResponse
  | pathInfo request == ["healthz"] = respond $ healthzResponseFor request
  | otherwise = serveStaticPath distDir request >>= respond

healthzResponse :: Response
healthzResponse = responseLBS status200 [("Content-Type", "text/plain")] "okay"

notFoundResponse :: Response
notFoundResponse = responseLBS status404 [] "not found"

methodNotAllowedResponse :: Response
methodNotAllowedResponse = responseLBS status405 [("Allow", "GET, HEAD")] ""

healthzResponseFor :: Request -> Response
healthzResponseFor request
  | requestMethod request == methodHead = responseLBS status200 [("Content-Type", "text/plain")] ""
  | otherwise = healthzResponse

serveStaticPath :: FilePath -> Request -> IO Response
serveStaticPath distDir request =
    case staticFilePath distDir $ pathInfo request of
        Nothing -> return notFoundResponse
        Just filePath -> do
            exists <- doesFileExist filePath
            return $ if exists
                then fileResponse filePath
                else notFoundResponse

staticFilePath :: FilePath -> [Text.Text] -> Maybe FilePath
staticFilePath distDir [] = Just $ distDir </> "index.html"
staticFilePath distDir segments = do
    safeSegments <- traverse safeSegment segments
    return $ foldl (</>) distDir safeSegments

safeSegment :: Text.Text -> Maybe FilePath
safeSegment segment
  | segment == "." = Nothing
  | segment == ".." = Nothing
  | Text.null segment = Nothing
  | Text.any isUnsafeSegmentChar segment = Nothing
  | otherwise = Just $ Text.unpack segment

isUnsafeSegmentChar :: Char -> Bool
isUnsafeSegmentChar char = char == '/' || char == '\\'

fileResponse :: FilePath -> Response
fileResponse filePath = responseFile status200 [("Content-Type", defaultMimeLookup $ Text.pack filePath)] filePath Nothing

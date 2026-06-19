module Network.OmpRelayWai.StaticSpec
  ( spec
  ) where

import Control.Exception  (bracket)
import Data.ByteString    qualified as BS
import Data.Unique        (hashUnique, newUnique)
import Network.HTTP.Types (hContentType, methodGet, methodPost, status200, status404, status405)
import Network.Mime       (defaultMimeLookup)
import Network.Wai        (Request(..))
import Network.Wai.Test   qualified as WaiTest
import System.Directory
    (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath    ((</>))

import Network.OmpRelayWai.Static

import Test.Hspec

spec :: Spec
spec = describe "Network.OmpRelayWai.Static" $ do
    it "serves /healthz" $
        withTempDist $ \distDir -> do
            response <- runStaticRequest distDir methodGet "/healthz"
            WaiTest.simpleStatus response `shouldBe` status200
            WaiTest.simpleBody response `shouldBe` "okay"

    it "serves / as index.html with the default content type" $
        withTempDist $ \distDir -> do
            response <- runStaticRequest distDir methodGet "/"
            WaiTest.simpleStatus response `shouldBe` status200
            WaiTest.simpleBody response `shouldBe` "<html>index</html>"
            lookup hContentType (WaiTest.simpleHeaders response) `shouldBe`
                Just (defaultMimeLookup "index.html")

    it "serves direct and nested files" $
        withTempDist $ \distDir -> do
            appResponse <- runStaticRequest distDir methodGet "/app.js"
            WaiTest.simpleStatus appResponse `shouldBe` status200
            WaiTest.simpleBody appResponse `shouldBe` "console.log('app');"

            nestedResponse <- runStaticRequest distDir methodGet "/nested/file.txt"
            WaiTest.simpleStatus nestedResponse `shouldBe` status200
            WaiTest.simpleBody nestedResponse `shouldBe` "nested text"

    it "returns 404 for missing files" $
        withTempDist $ \distDir -> do
            response <- runStaticRequest distDir methodGet "/missing.txt"
            WaiTest.simpleStatus response `shouldBe` status404
            WaiTest.simpleBody response `shouldBe` "not found"

    it "returns 404 for unsafe path segments" $
        withTempDist $ \distDir -> do
            let request = WaiTest.defaultRequest
                    { requestMethod = methodGet
                    , rawPathInfo = "/../index.html"
                    , pathInfo = ["..", "index.html"]
                    }
            response <- WaiTest.runSession (WaiTest.request request) (staticDistApp distDir)
            WaiTest.simpleStatus response `shouldBe` status404
            WaiTest.simpleBody response `shouldBe` "not found"

    it "returns 405 for unsupported methods" $
        withTempDist $ \distDir -> do
            healthResponse <- runStaticRequest distDir methodPost "/healthz"
            WaiTest.simpleStatus healthResponse `shouldBe` status405
            lookup "Allow" (WaiTest.simpleHeaders healthResponse) `shouldBe` Just "GET, HEAD"

            indexResponse <- runStaticRequest distDir methodPost "/"
            WaiTest.simpleStatus indexResponse `shouldBe` status405
            lookup "Allow" (WaiTest.simpleHeaders indexResponse) `shouldBe` Just "GET, HEAD"

withTempDist :: (FilePath -> IO a) -> IO a
withTempDist action = bracket createTempDist removePathForcibly $ \distDir -> do
    BS.writeFile (distDir </> "index.html") "<html>index</html>"
    BS.writeFile (distDir </> "app.js") "console.log('app');"
    createDirectoryIfMissing True $ distDir </> "nested"
    BS.writeFile (distDir </> "nested" </> "file.txt") "nested text"
    action distDir

createTempDist :: IO FilePath
createTempDist = do
    tmpDir <- getTemporaryDirectory
    unique <- hashUnique <$> newUnique
    let distDir = tmpDir </> ("omp-relay-wai-test-" <> show unique)
    createDirectory distDir
    return distDir

runStaticRequest :: FilePath -> BS.ByteString -> BS.ByteString -> IO WaiTest.SResponse
runStaticRequest distDir method path =
    WaiTest.runSession (WaiTest.request request) (staticDistApp distDir)
  where
    request = (WaiTest.setPath WaiTest.defaultRequest path)
        { requestMethod = method
        }

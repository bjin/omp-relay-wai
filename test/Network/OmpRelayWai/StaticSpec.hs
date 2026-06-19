-- SPDX-License-Identifier: Apache-2.0
--
-- Copyright (C) 2026 Bin Jin. All Rights Reserved.

module Network.OmpRelayWai.StaticSpec
  ( spec
  ) where

import Data.ByteString       qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy  qualified as LBS
import Network.HTTP.Types
    (hContentLength, hContentType, methodGet, methodHead, methodPost, status200, status404,
    status405)
import Network.Mime          (defaultMimeLookup)
import Network.Wai           (Application, Request(..))
import Network.Wai.Test      qualified as WaiTest
import Test.Hspec

import Network.OmpRelayWai        (app, newRelayState)
import Network.OmpRelayWai.Static

-- | Static file application behavior.
spec :: Spec
spec = describe "Network.OmpRelayWai.Static" $ do
    let fixtureApp = staticFilesApp fixtureFiles

    it "serves /healthz" $ do
        response <- runStaticRequest fixtureApp methodGet "/healthz"
        WaiTest.simpleStatus response `shouldBe` status200
        WaiTest.simpleBody response `shouldBe` "okay"

        headResponse <- runStaticRequest fixtureApp methodHead "/healthz"
        WaiTest.simpleStatus headResponse `shouldBe` status200
        WaiTest.simpleBody headResponse `shouldBe` ""

    it "serves / as index.html with the default content type" $ do
        response <- runStaticRequest fixtureApp methodGet "/"
        WaiTest.simpleStatus response `shouldBe` status200
        WaiTest.simpleBody response `shouldBe` "<html>index</html>"
        lookup hContentType (WaiTest.simpleHeaders response) `shouldBe`
            Just (defaultMimeLookup "index.html")

    it "serves direct and nested files" $ do
        appResponse <- runStaticRequest fixtureApp methodGet "/app.js"
        WaiTest.simpleStatus appResponse `shouldBe` status200
        WaiTest.simpleBody appResponse `shouldBe` "console.log('app');"

        nestedResponse <- runStaticRequest fixtureApp methodGet "/nested/file.txt"
        WaiTest.simpleStatus nestedResponse `shouldBe` status200
        WaiTest.simpleBody nestedResponse `shouldBe` "nested text"

    it "returns 404 for missing files" $ do
        response <- runStaticRequest fixtureApp methodGet "/missing.txt"
        WaiTest.simpleStatus response `shouldBe` status404
        WaiTest.simpleBody response `shouldBe` "not found"

    it "returns 404 for unsafe path segments" $ do
        let request = WaiTest.defaultRequest
                { requestMethod = methodGet
                , rawPathInfo   = "/../index.html"
                , pathInfo      = ["..", "index.html"]
                }
        response <- WaiTest.runSession (WaiTest.request request) fixtureApp
        WaiTest.simpleStatus response `shouldBe` status404
        WaiTest.simpleBody response `shouldBe` "not found"

    it "returns 404 for dot-prefixed path segments" $
        mapM_
            (\path -> do
                response <- runStaticRequest fixtureApp methodGet path
                WaiTest.simpleStatus response `shouldBe` status404
                WaiTest.simpleBody response `shouldBe` "not found")
            ["/.env", "/.hidden/folder.png", "/foo/.bar"]

    it "returns 404 for directory requests" $ do
        response <- runStaticRequest fixtureApp methodGet "/nested"
        WaiTest.simpleStatus response `shouldBe` status404
        WaiTest.simpleBody response `shouldBe` "not found"

    it "returns 405 for unsupported methods" $ do
        healthResponse <- runStaticRequest fixtureApp methodPost "/healthz"
        WaiTest.simpleStatus healthResponse `shouldBe` status405
        lookup "Allow" (WaiTest.simpleHeaders healthResponse) `shouldBe` Just "GET, HEAD"

        indexResponse <- runStaticRequest fixtureApp methodPost "/"
        WaiTest.simpleStatus indexResponse `shouldBe` status405
        lookup "Allow" (WaiTest.simpleHeaders indexResponse) `shouldBe` Just "GET, HEAD"

    it "serves embedded assets with explicit content length" $ do
        indexResponse <- runStaticRequest fixtureApp methodGet "/"
        shouldHaveContentLength "<html>index</html>" indexResponse

        appResponse <- runStaticRequest fixtureApp methodGet "/app.js"
        shouldHaveContentLength "console.log('app');" appResponse

        headResponse <- runStaticRequest fixtureApp methodHead "/app.js"
        WaiTest.simpleBody headResponse `shouldBe` ""
        shouldHaveContentLength "console.log('app');" headResponse

    it "allows cross-origin mode asset loads" $ do
        indexResponse <- runStaticRequest fixtureApp methodGet "/"
        shouldHaveCorsAccess indexResponse

        appResponse <- runStaticRequest fixtureApp methodGet "/app.js"
        shouldHaveCorsAccess appResponse

    it "serves / from production embedded assets" $ do
        response <- runStaticRequest staticDistApp methodGet "/"
        WaiTest.simpleStatus response `shouldBe` status200
        lookup hContentType (WaiTest.simpleHeaders response) `shouldBe`
            Just (defaultMimeLookup "index.html")
        WaiTest.simpleBody response `shouldSatisfy` not . LBS.null

    it "serves /healthz through the public application" $ do
        state <- newRelayState
        response <- runStaticRequest (app state) methodGet "/healthz"
        WaiTest.simpleStatus response `shouldBe` status200
        WaiTest.simpleBody response `shouldBe` "okay"

fixtureFiles :: [(FilePath, BS.ByteString)]
fixtureFiles =
    [ ("index.html", "<html>index</html>")
    , ("app.js", "console.log('app');")
    , ("nested/file.txt", "nested text")
    ]

shouldHaveContentLength :: BS.ByteString -> WaiTest.SResponse -> Expectation
shouldHaveContentLength content response =
    lookup hContentLength (WaiTest.simpleHeaders response) `shouldBe`
        Just (BS8.pack $ show $ BS.length content)

shouldHaveCorsAccess :: WaiTest.SResponse -> Expectation
shouldHaveCorsAccess response =
    lookup "Access-Control-Allow-Origin" (WaiTest.simpleHeaders response) `shouldBe` Just "*"

runStaticRequest :: Application -> BS.ByteString -> BS.ByteString -> IO WaiTest.SResponse
runStaticRequest testApp method path =
    WaiTest.runSession (WaiTest.request request) testApp
  where
    request = (WaiTest.setPath WaiTest.defaultRequest path)
        { requestMethod = method
        }

-- SPDX-License-Identifier: Apache-2.0
--
-- Copyright (C) 2026 Bin Jin. All Rights Reserved.

module Network.OmpRelayWai.RelaySpec
  ( spec
  ) where

import Control.Exception        (try)
import Control.Monad            (forM_)
import Data.ByteString          qualified as BS
import Data.ByteString.Lazy     qualified as LBS
import Data.Word                (Word16)
import Network.HTTP.Client
    (Response, defaultManagerSettings, httpLbs, newManager, parseRequest, responseBody,
    responseStatus)
import Network.HTTP.Types       (mkStatus, status404)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.WebSockets       qualified as WS
import System.Timeout           (timeout)
import Test.Hspec

import Network.OmpRelayWai       (RelayConfig(..), app, defaultRelayConfig, newRelayStateWith)
import Network.OmpRelayWai.Relay
    (EnqueueResult(..), Outbound(..), enqueueOutbound, newOutboundQueue, requestClose)

-- | WebSocket relay integration behavior.
spec :: Spec
spec = describe "Network.OmpRelayWai.Relay" $ do
    it "closes a guest that joins before the host" $
        withRelay $ \port ->
            runClient port guestPath $ \guest ->
                expectClose 4004 "no such room" guest

    it "closes a second host for the same room" $
        withRelay $ \port ->
            runClient port hostPath $ \_host ->
                runClient port hostPath $ \duplicate ->
                    expectClose 4009 "a host is already connected for this room" duplicate

    it "joins guests, rewrites guest frames, broadcasts, and targets host frames" $
        withRelay $ \port ->
            runClient port hostPath $ \host ->
                runClient port guestPath $ \guest1 -> do
                    expectText host "{\"t\":\"peer-joined\",\"peer\":1}"
                    runClient port guestPath $ \guest2 -> do
                        expectText host "{\"t\":\"peer-joined\",\"peer\":2}"

                        WS.sendBinaryData guest1 (BS.pack [0, 0, 0, 0, 1, 2, 3])
                        expectBinary host $ BS.pack [0, 0, 0, 1, 1, 2, 3]

                        let broadcast = BS.pack [0, 0, 0, 0, 4, 5, 6]
                        WS.sendBinaryData host broadcast
                        expectBinary guest1 broadcast
                        expectBinary guest2 broadcast

                        let targeted = BS.pack [0, 0, 0, 2, 7, 8, 9]
                        WS.sendBinaryData host targeted
                        expectBinary guest2 targeted
                        expectNoDataMessage guest1

    it "notifies the host when a guest closes" $
        withRelay $ \port ->
            runClient port hostPath $ \host ->
                runClient port guestPath $ \guest -> do
                    expectText host "{\"t\":\"peer-joined\",\"peer\":1}"
                    WS.sendClose guest ("done" :: BS.ByteString)
                    expectText host "{\"t\":\"peer-left\",\"peer\":1}"

    it "notifies and closes guests when the host closes" $
        withRelay $ \port ->
            runClient port hostPath $ \host ->
                runClient port guestPath $ \guest1 -> do
                    expectText host "{\"t\":\"peer-joined\",\"peer\":1}"
                    runClient port guestPath $ \guest2 -> do
                        expectText host "{\"t\":\"peer-joined\",\"peer\":2}"
                        WS.sendClose host ("done" :: BS.ByteString)
                        expectText guest1 "{\"t\":\"room-closed\"}"
                        expectClose 4001 "room closed" guest1
                        expectText guest2 "{\"t\":\"room-closed\"}"
                        expectClose 4001 "room closed" guest2

    it "returns 426 for valid non-upgrade relay requests" $
        withRelay $ \port -> do
            response <- httpGet port hostPath
            responseStatus response `shouldBe` mkStatus 426 "Upgrade Required"
            responseBody response `shouldBe` "websocket upgrade required"

    it "returns 404 for invalid relay routes" $
        withRelay $ \port -> do
            response <- httpGet port "/r/short?role=host"
            responseStatus response `shouldBe` status404
            responseBody response `shouldBe` "not found"

    it "disconnects a client that exceeds the message size limit" $
        withRelayConfig defaultRelayConfig { relayMaxMessageBytes = 64 * 1024 } $ \port ->
            runClient port hostPath $ \host ->
                runClient port guestPath $ \guest -> do
                    expectText host "{\"t\":\"peer-joined\",\"peer\":1}"
                    WS.sendBinaryData guest $ BS.replicate (64 * 1024 + 5) 0x41
                    expectText host "{\"t\":\"peer-left\",\"peer\":1}"
                    expectAbruptClose guest

    it "pings clients at the configured interval" $
        withRelayConfig defaultRelayConfig { relayPingIntervalSeconds = 1 } $ \port ->
            runClient port hostPath $ \host -> do
                pinged <- timeout 3000000 $ waitForPing host
                pinged `shouldBe` Just ()

    it "ejects a backlogged guest without stalling the room" $ do
        let config = defaultRelayConfig
                { relayMaxSendQueueBytes  = 256 * 1024
                , relayMaxSendQueueLength = 512
                }
        withRelayConfig config $ \port ->
            runClient port hostPath $ \host ->
                runClient port guestPath $ \slowGuest -> do
                    expectText host "{\"t\":\"peer-joined\",\"peer\":1}"
                    runClient port guestPath $ \liveGuest -> do
                        expectText host "{\"t\":\"peer-joined\",\"peer\":2}"
                        -- Each frame (64 KiB + 4 B header) is far below the
                        -- 256 KiB threshold; only cumulative backlog at the
                        -- never-reading slowGuest can overflow it.
                        let targeted = BS.pack [0, 0, 0, 1] <> BS.replicate 65536 0x42
                        forM_ [1 :: Int .. 512] $ \_ -> WS.sendBinaryData host targeted
                        expectTextWithin 10000000 host "{\"t\":\"peer-left\",\"peer\":1}"
                        let broadcast = BS.pack [0, 0, 0, 0, 7, 8, 9]
                        WS.sendBinaryData host broadcast
                        expectBinary liveGuest broadcast
                        expectEventualClose slowGuest

    describe "outbound backlog" $ do
        it "admits messages until the existing backlog exceeds the byte cap" $ do
            outbound <- newOutboundQueue defaultRelayConfig
                { relayMaxSendQueueBytes  = 100
                , relayMaxSendQueueLength = 8
                }
            let frame = OutboundBinary (BS.replicate 40 0x00)
            results <- mapM (enqueueOutbound outbound) [frame, frame, frame, frame]
            results `shouldBe` [Enqueued, Enqueued, Enqueued, Overflow]

        it "admits one message of any size into an empty backlog" $ do
            outbound <- newOutboundQueue defaultRelayConfig
                { relayMaxSendQueueBytes  = 100
                , relayMaxSendQueueLength = 8
                }
            result <- enqueueOutbound outbound (OutboundBinary (BS.replicate 4096 0x00))
            result `shouldBe` Enqueued

        it "overflows when the queue length cap is hit" $ do
            outbound <- newOutboundQueue defaultRelayConfig
                { relayMaxSendQueueBytes  = 1000000
                , relayMaxSendQueueLength = 2
                }
            let msg = OutboundText "y"
            results <- mapM (enqueueOutbound outbound) [msg, msg, msg]
            results `shouldBe` [Enqueued, Enqueued, Overflow]

        it "drops messages once a close is requested" $ do
            outbound <- newOutboundQueue defaultRelayConfig
            requestClose outbound 4001 "room closed"
            result <- enqueueOutbound outbound (OutboundText "x")
            result `shouldBe` Dropped

roomId :: String
roomId = "AbCdEf123456_-Xy"

hostPath :: String
hostPath = "/r/" <> roomId <> "?role=host"

guestPath :: String
guestPath = "/r/" <> roomId <> "?role=guest"

withRelay :: (Int -> IO ()) -> IO ()
withRelay = withRelayConfig defaultRelayConfig

withRelayConfig :: RelayConfig -> (Int -> IO ()) -> IO ()
withRelayConfig config action = do
    state <- newRelayStateWith config
    testWithApplication (return $ app state) action

runClient :: Int -> String -> WS.ClientApp a -> IO a
runClient port path = WS.runClient "127.0.0.1" port path

httpGet :: Int -> String -> IO (Response LBS.ByteString)
httpGet port path = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest $ "http://127.0.0.1:" <> show port <> path
    httpLbs request manager

expectText :: WS.Connection -> BS.ByteString -> IO ()
expectText conn expected = do
    message <- WS.receiveDataMessage conn
    case message of
        WS.Text actual _ -> LBS.toStrict actual `shouldBe` expected
        _                -> expectationFailure "expected text websocket message"

expectBinary :: WS.Connection -> BS.ByteString -> IO ()
expectBinary conn expected = do
    message <- WS.receiveDataMessage conn
    case message of
        WS.Binary actual -> LBS.toStrict actual `shouldBe` expected
        _                -> expectationFailure "expected binary websocket message"

expectNoDataMessage :: WS.Connection -> IO ()
expectNoDataMessage conn = do
    message <- timeout 100000 $ WS.receiveDataMessage conn
    case message of
        Nothing -> return ()
        Just _  -> expectationFailure "received unexpected websocket message"

expectClose :: Word16 -> BS.ByteString -> WS.Connection -> IO ()
expectClose expectedCode expectedReason conn = do
    result <- receiveDataOrClose conn
    case result of
        Left (WS.CloseRequest actualCode actualReason) -> do
            actualCode `shouldBe` expectedCode
            LBS.toStrict actualReason `shouldBe` expectedReason
        Left _  -> expectationFailure "websocket closed without a close frame"
        Right _ -> expectationFailure "received data message instead of websocket close"

receiveDataOrClose :: WS.Connection -> IO (Either WS.ConnectionException WS.DataMessage)
receiveDataOrClose conn = try $ WS.receiveDataMessage conn

expectAbruptClose :: WS.Connection -> IO ()
expectAbruptClose conn = do
    result <- receiveDataOrClose conn
    case result of
        Left _  -> return ()
        Right _ -> expectationFailure "expected the relay to drop the connection"

expectTextWithin :: Int -> WS.Connection -> BS.ByteString -> IO ()
expectTextWithin micros conn expected = do
    result <- timeout micros $ WS.receiveDataMessage conn
    case result of
        Nothing                 -> expectationFailure "timed out waiting for text message"
        Just (WS.Text actual _) -> LBS.toStrict actual `shouldBe` expected
        Just _                  -> expectationFailure "expected text websocket message"

-- | Drain any data frames delivered before the eject, then require the
-- connection to die. Bounded so a regression cannot hang the suite.
expectEventualClose :: WS.Connection -> IO ()
expectEventualClose conn = do
    outcome <- timeout 10000000 drainUntilClose
    case outcome of
        Nothing -> expectationFailure "timed out waiting for the relay to drop the connection"
        Just () -> return ()
  where
    drainUntilClose = do
        result <- receiveDataOrClose conn
        case result of
            Left _  -> return ()
            Right _ -> drainUntilClose

waitForPing :: WS.Connection -> IO ()
waitForPing conn = do
    message <- WS.receive conn
    case message of
        WS.ControlMessage (WS.Ping _) -> return ()
        _                             -> waitForPing conn

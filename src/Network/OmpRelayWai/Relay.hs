-- SPDX-License-Identifier: Apache-2.0
--
-- Copyright (C) 2026 Bin Jin. All Rights Reserved.

module Network.OmpRelayWai.Relay
  ( ClientRole(..)
  , RelayConfig(..)
  , RelayRequest(..)
  , RelayState
  , RoomId(..)
  , defaultRelayConfig
  , newRelayState
  , newRelayStateWith
  , parseRelayRequest
  , relayConnectionOptions
  , relayHttpFallback
  , relayServerApp
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Exception       (finally, handle)
import Control.Monad           (forM_, forever, when)
import Data.ByteString         qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy    qualified as LBS
import Data.Hashable           (Hashable(..))
import Data.HashMap.Strict     qualified as HashMap
import Data.Int                (Int64)
import Data.IntMap.Strict      qualified as IntMap
import Data.List               (find)
import Data.Unique             (Unique, newUnique)
import Data.Word               (Word16, Word32, Word64, Word8)
import Network.HTTP.Types      (mkStatus)
import Network.HTTP.Types.URI  (parseQuery)
import Network.Wai             (Application, Response, rawPathInfo, rawQueryString, responseLBS)
import Network.WebSockets      qualified as WS

import Network.OmpRelayWai.Envelope

-- | Mutable state for active relay rooms.
data RelayState = RelayState
    { relayRooms  :: !(MVar (HashMap.HashMap RoomId Room))
    , relayConfig :: !RelayConfig
    }

-- | Tunable relay limits. Production uses 'defaultRelayConfig'; tests shrink
-- the values to exercise limit behavior quickly.
data RelayConfig = RelayConfig
    { relayPingIntervalSeconds :: !Int
      -- ^ Server-initiated WebSocket ping period. Must stay well below warp's
      -- 30-second slowloris timeout, which stays armed during raw sessions.
    , relayMaxMessageBytes     :: !Int64
      -- ^ Incoming frame and message size limit (matches Bun's default
      -- @maxPayloadLength@ on the reference relay).
    , relayMaxSendQueueBytes   :: !Int
      -- ^ Per-client outbound backlog cap in payload bytes; exceeding it
      -- force-disconnects the client (uWS @maxBackpressure@ analog).
    , relayMaxSendQueueLength  :: !Int
      -- ^ Per-client outbound backlog cap in messages.
    }
  deriving (Eq, Show)

-- | Production relay limits.
defaultRelayConfig :: RelayConfig
defaultRelayConfig = RelayConfig
    { relayPingIntervalSeconds = 15
    , relayMaxMessageBytes     = 16 * 1024 * 1024
    , relayMaxSendQueueBytes   = 4 * 1024 * 1024
    , relayMaxSendQueueLength  = 4096
    }

-- | Room key accepted in @/r/<roomId>@ relay routes.
newtype RoomId = RoomId { unRoomId :: BS.ByteString }
  deriving (Eq, Show)

instance Hashable RoomId where
    hashWithSalt salt (RoomId roomId) = hashWithSalt salt roomId

-- | Role requested by a WebSocket client.
data ClientRole = HostRole | GuestRole
  deriving (Eq, Show)

-- | Parsed relay route with room and role.
data RelayRequest = RelayRequest
    { relayRequestRoomId :: !RoomId
    , relayRequestRole   :: !ClientRole
    }
  deriving (Eq, Show)

data Room = Room
    { roomToken  :: !Unique
    , roomHost   :: !RelayClient
    , roomGuests :: !(MVar RoomGuests)
    }

data RoomGuests = RoomGuests
    { roomGuestMap   :: !(IntMap.IntMap RelayClient)
    , roomNextPeerId :: !Word64
    }

data RelayClient = RelayClient
    { relayClientPeerId     :: !PeerId
    , relayClientConnection :: !WS.Connection
    , relayClientSendLock   :: !(MVar ())
    }

-- | Allocate an empty relay state with production limits.
newRelayState :: IO RelayState
newRelayState = newRelayStateWith defaultRelayConfig

-- | Allocate an empty relay state with explicit limits.
newRelayStateWith :: RelayConfig -> IO RelayState
newRelayStateWith config = do
    rooms <- newMVar HashMap.empty
    return RelayState
        { relayRooms  = rooms
        , relayConfig = config
        }

-- | WebSocket options for relay connections: cap incoming frame and message
-- sizes so an anonymous client cannot exhaust relay memory
-- ('WS.defaultConnectionOptions' imposes no limit at all).
relayConnectionOptions :: RelayState -> WS.ConnectionOptions
relayConnectionOptions RelayState{relayConfig = RelayConfig{..}} =
    WS.defaultConnectionOptions
        { WS.connectionFramePayloadSizeLimit = WS.SizeLimit relayMaxMessageBytes
        , WS.connectionMessageDataSizeLimit  = WS.SizeLimit relayMaxMessageBytes
        }

-- | Parse a relay route and role from WAI path/query bytes.
parseRelayRequest :: BS.ByteString -> BS.ByteString -> Maybe RelayRequest
parseRelayRequest path rawQuery = do
    roomId <- parseRelayRoomPath path
    role <- parseRole $ stripLeadingQuestion rawQuery
    return RelayRequest
        { relayRequestRoomId = roomId
        , relayRequestRole   = role
        }

-- | Serve relay WebSocket requests.
relayServerApp :: RelayState -> WS.ServerApp
relayServerApp state pending =
    case parseRelayPendingRequest pending of
        Nothing -> rejectNotFound pending
        Just RelayRequest{..} -> do
            conn <- WS.acceptRequest pending
            client <- newRelayClient (PeerId 0) conn
            case relayRequestRole of
                HostRole  -> openHost state relayRequestRoomId client
                GuestRole -> openGuest state relayRequestRoomId client

-- | Return the relay-specific response for non-WebSocket HTTP requests.
relayHttpFallback :: Application
relayHttpFallback request respond =
    respond $ case parseRelayRequest (rawPathInfo request) (rawQueryString request) of
        Just _  -> upgradeRequiredResponse
        Nothing -> notFoundResponse

parseRelayPendingRequest :: WS.PendingConnection -> Maybe RelayRequest
parseRelayPendingRequest pending =
    let (path, query) = splitRequestTarget $ WS.requestPath $ WS.pendingRequest pending
    in parseRelayRequest path query

splitRequestTarget :: BS.ByteString -> (BS.ByteString, BS.ByteString)
splitRequestTarget target = BS.break (== questionMark) target
  where
    questionMark = 63

parseRelayRoomPath :: BS.ByteString -> Maybe RoomId
parseRelayRoomPath path = do
    roomId <- BS.stripPrefix "/r/" path
    if validRoomId roomId
    then Just $ RoomId roomId
    else Nothing

validRoomId :: BS.ByteString -> Bool
validRoomId roomId =
    let len = BS.length roomId
    in len >= 10 && len <= 64 && BS.all isRoomIdByte roomId

isRoomIdByte :: Word8 -> Bool
isRoomIdByte byte =
    byte >= 65 && byte <= 90
    || byte >= 97 && byte <= 122
    || byte >= 48 && byte <= 57
    || byte == 95
    || byte == 45

stripLeadingQuestion :: BS.ByteString -> BS.ByteString
stripLeadingQuestion rawQuery = case BS.stripPrefix "?" rawQuery of
    Just query -> query
    Nothing    -> rawQuery

parseRole :: BS.ByteString -> Maybe ClientRole
parseRole query = do
    roleParam <- find ((== "role") . fst) $ parseQuery query
    case roleParam of
        (_, Just "host")  -> Just HostRole
        (_, Just "guest") -> Just GuestRole
        _                 -> Nothing

openHost :: RelayState -> RoomId -> RelayClient -> IO ()
openHost state@RelayState{..} roomId host = do
    room <- createRoom host
    inserted <- modifyMVar relayRooms $ \rooms ->
        if HashMap.member roomId rooms
        then return (rooms, False)
        else return (HashMap.insert roomId room rooms, True)
    if inserted
    then runClientUntilClose (cleanupHost state roomId room) $ hostReceiveLoop room
    else closeClient host 4009 "a host is already connected for this room"

openGuest :: RelayState -> RoomId -> RelayClient -> IO ()
openGuest state roomId client = do
    room <- lookupRoom state roomId
    case room of
        Nothing -> closeClient client 4004 "no such room"
        Just liveRoom -> do
            guest <- insertGuest liveRoom client
            case guest of
                Nothing -> closeClient client 1011 "peer id exhausted"
                Just guestClient -> do
                    live <- roomStillLive state roomId liveRoom
                    if live
                    then do
                        sendTextClient (roomHost liveRoom) $
                            peerJoinedMessage (relayClientPeerId guestClient)
                        runClientUntilClose (cleanupGuest state roomId liveRoom guestClient) $
                            guestReceiveLoop liveRoom guestClient
                    else do
                        removeGuestSilently liveRoom guestClient
                        closeClient guestClient 4001 "room closed"

createRoom :: RelayClient -> IO Room
createRoom host = do
    token <- newUnique
    guests <- newMVar RoomGuests
        { roomGuestMap   = IntMap.empty
        , roomNextPeerId = 1
        }
    return Room
        { roomToken  = token
        , roomHost   = host
        , roomGuests = guests
        }

newRelayClient :: PeerId -> WS.Connection -> IO RelayClient
newRelayClient peerId conn = do
    sendLock <- newMVar ()
    return RelayClient
        { relayClientPeerId     = peerId
        , relayClientConnection = conn
        , relayClientSendLock   = sendLock
        }

lookupRoom :: RelayState -> RoomId -> IO (Maybe Room)
lookupRoom RelayState{..} roomId = withMVar relayRooms $ \rooms ->
    return $ HashMap.lookup roomId rooms

roomStillLive :: RelayState -> RoomId -> Room -> IO Bool
roomStillLive RelayState{..} roomId room = withMVar relayRooms $ \rooms ->
    return $ case HashMap.lookup roomId rooms of
        Just current -> roomToken current == roomToken room
        Nothing      -> False

insertGuest :: Room -> RelayClient -> IO (Maybe RelayClient)
insertGuest Room{..} client = modifyMVar roomGuests $ \guests@RoomGuests{..} ->
    if roomNextPeerId > fromIntegral (maxBound :: Word32)
    then return (guests, Nothing)
    else do
        let peerId     = PeerId $ fromIntegral roomNextPeerId
            guest      = client { relayClientPeerId = peerId }
            nextGuests = RoomGuests
                { roomGuestMap   = IntMap.insert (peerIdKey peerId) guest roomGuestMap
                , roomNextPeerId = roomNextPeerId + 1
                }
        return (nextGuests, Just guest)

removeGuestSilently :: Room -> RelayClient -> IO ()
removeGuestSilently Room{..} RelayClient{..} = modifyMVar roomGuests $ \guests@RoomGuests{..} ->
    let nextGuests = guests
            { roomGuestMap = IntMap.delete (peerIdKey relayClientPeerId) roomGuestMap
            }
    in return (nextGuests, ())

hostReceiveLoop :: Room -> IO ()
hostReceiveLoop room@Room{..} = forever $ do
    message <- WS.receiveDataMessage $ relayClientConnection roomHost
    case message of
        WS.Text _ _   -> return ()
        WS.Binary raw -> handleHostBinary room $ LBS.toStrict raw

guestReceiveLoop :: Room -> RelayClient -> IO ()
guestReceiveLoop room guest = forever $ do
    message <- WS.receiveDataMessage $ relayClientConnection guest
    case message of
        WS.Text _ _   -> return ()
        WS.Binary raw -> handleGuestBinary room guest $ LBS.toStrict raw

handleHostBinary :: Room -> BS.ByteString -> IO ()
handleHostBinary Room{..} message =
    case parseEnvelope message of
        Nothing -> return ()
        Just Envelope{..}
          | envelopePeerId == PeerId 0 -> do
                guests <- snapshotGuests roomGuests
                forM_ guests $ \guest -> sendBinaryClient guest message
          | otherwise -> do
                guest <- lookupGuest roomGuests envelopePeerId
                forM_ guest $ \target -> sendBinaryClient target message

handleGuestBinary :: Room -> RelayClient -> BS.ByteString -> IO ()
handleGuestBinary Room{..} RelayClient{..} message =
    forM_ (rewriteEnvelopePeer relayClientPeerId message) $ sendBinaryClient roomHost

snapshotGuests :: MVar RoomGuests -> IO [RelayClient]
snapshotGuests guestsVar = withMVar guestsVar $ \RoomGuests{..} ->
    return $ IntMap.elems roomGuestMap

lookupGuest :: MVar RoomGuests -> PeerId -> IO (Maybe RelayClient)
lookupGuest guestsVar peerId = withMVar guestsVar $ \RoomGuests{..} ->
    return $ IntMap.lookup (peerIdKey peerId) roomGuestMap

cleanupHost :: RelayState -> RoomId -> Room -> IO ()
cleanupHost RelayState{..} roomId room@Room{ roomGuests = guestsVar } = do
    removed <- modifyMVar relayRooms $ \rooms ->
        case HashMap.lookup roomId rooms of
            Just current | roomToken current == roomToken room ->
                return (HashMap.delete roomId rooms, True)
            _ -> return (rooms, False)
    when removed $ do
        guests <- modifyMVar guestsVar $ \RoomGuests{..} ->
            let nextGuests = RoomGuests
                    { roomGuestMap   = IntMap.empty
                    , roomNextPeerId = roomNextPeerId
                    }
            in return (nextGuests, IntMap.elems roomGuestMap)
        forM_ guests closeGuestForRoomClosed

cleanupGuest :: RelayState -> RoomId -> Room -> RelayClient -> IO ()
cleanupGuest state roomId room@Room{..} RelayClient{..} = do
    removed <- modifyMVar roomGuests $ \guests@RoomGuests{..} ->
        let key        = peerIdKey relayClientPeerId
            wasPresent = IntMap.member key roomGuestMap
            nextGuests = guests { roomGuestMap = IntMap.delete key roomGuestMap }
        in return (nextGuests, wasPresent)
    live <- roomStillLive state roomId room
    when (removed && live) $
        sendTextClient roomHost $ peerLeftMessage relayClientPeerId

closeGuestForRoomClosed :: RelayClient -> IO ()
closeGuestForRoomClosed guest =
    sendClient guest $ \conn -> do
        WS.sendTextData conn roomClosedMessage
        WS.sendCloseCode conn 4001 ("room closed" :: BS.ByteString)

runClientUntilClose :: IO () -> IO () -> IO ()
runClientUntilClose cleanup action =
    handleConnectionException action `finally` cleanup

sendTextClient :: RelayClient -> BS.ByteString -> IO ()
sendTextClient client message = sendClient client $ \conn ->
    WS.sendTextData conn message

sendBinaryClient :: RelayClient -> BS.ByteString -> IO ()
sendBinaryClient client message = sendClient client $ \conn ->
    WS.sendBinaryData conn message

closeClient :: RelayClient -> Word16 -> BS.ByteString -> IO ()
closeClient client code reason = sendClient client $ \conn ->
    WS.sendCloseCode conn code reason

sendClient :: RelayClient -> (WS.Connection -> IO ()) -> IO ()
sendClient RelayClient{..} action = handleConnectionException $
    withMVar relayClientSendLock $ \() -> action relayClientConnection

handleConnectionException :: IO () -> IO ()
handleConnectionException = handle ignoreConnectionException

ignoreConnectionException :: WS.ConnectionException -> IO ()
ignoreConnectionException _ = return ()

rejectNotFound :: WS.PendingConnection -> IO ()
rejectNotFound pending = WS.rejectRequestWith pending WS.defaultRejectRequest
    { WS.rejectCode    = 404
    , WS.rejectMessage = "Not Found"
    , WS.rejectBody    = "not found"
    }

notFoundResponse :: Response
notFoundResponse = responseLBS (mkStatus 404 "Not Found") [] "not found"

upgradeRequiredResponse :: Response
upgradeRequiredResponse =
    responseLBS (mkStatus 426 "Upgrade Required") [] "websocket upgrade required"

peerJoinedMessage :: PeerId -> BS.ByteString
peerJoinedMessage = peerMessage "peer-joined"

peerLeftMessage :: PeerId -> BS.ByteString
peerLeftMessage = peerMessage "peer-left"

peerMessage :: BS.ByteString -> PeerId -> BS.ByteString
peerMessage tag (PeerId peerId) = LBS.toStrict $ Builder.toLazyByteString $
    Builder.byteString "{\"t\":\""
    <> Builder.byteString tag
    <> Builder.byteString "\",\"peer\":"
    <> Builder.word32Dec peerId
    <> Builder.char8 '}'

roomClosedMessage :: BS.ByteString
roomClosedMessage = "{\"t\":\"room-closed\"}"

peerIdKey :: PeerId -> Int
peerIdKey (PeerId peerId) = fromIntegral peerId

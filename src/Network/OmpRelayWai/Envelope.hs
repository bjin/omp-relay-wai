module Network.OmpRelayWai.Envelope
  ( Envelope(..)
  , PeerId(..)
  , envelopeHeaderLength
  , parseEnvelope
  , renderEnvelope
  , rewriteEnvelopePeer
  ) where

import Data.Bits       (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.Word       (Word32)

data Envelope = Envelope
    { envelopePeerId  :: !PeerId
    , envelopePayload :: !BS.ByteString
    } deriving (Eq, Show)

newtype PeerId = PeerId { unPeerId :: Word32 }
    deriving (Eq, Ord, Show)

envelopeHeaderLength :: Int
envelopeHeaderLength = 4

parseEnvelope :: BS.ByteString -> Maybe Envelope
parseEnvelope bytes
  | BS.length bytes < envelopeHeaderLength = Nothing
  | otherwise = Just Envelope
      { envelopePeerId = parsePeerId bytes
      , envelopePayload = BS.drop envelopeHeaderLength bytes
      }

renderEnvelope :: PeerId -> BS.ByteString -> BS.ByteString
renderEnvelope peerId payload = renderPeerId peerId <> payload

rewriteEnvelopePeer :: PeerId -> BS.ByteString -> Maybe BS.ByteString
rewriteEnvelopePeer peerId bytes
  | BS.length bytes < envelopeHeaderLength = Nothing
  | otherwise = Just $ renderEnvelope peerId (BS.drop envelopeHeaderLength bytes)

parsePeerId :: BS.ByteString -> PeerId
parsePeerId bytes = PeerId $
    shiftL (byteAt 0) 24
    .|. shiftL (byteAt 1) 16
    .|. shiftL (byteAt 2) 8
    .|. byteAt 3
  where
    byteAt = fromIntegral . BS.index bytes

renderPeerId :: PeerId -> BS.ByteString
renderPeerId (PeerId peerId) = BS.pack
    [ byteAt 24
    , byteAt 16
    , byteAt 8
    , byteAt 0
    ]
  where
    byteAt offset = fromIntegral $ shiftR peerId offset .&. 0xff

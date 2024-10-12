{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Event
  ( Event(..)
  , EventId(..)
  , encodeEvent
  , parseEvent
  ) where

import Data.ByteString
import Data.ProtocolBuffers
import Data.Serialize
import Data.UUID
import GHC.Generics

import CROSSMAP.User


newtype EventId = EventId { unEventId :: UUID } deriving (Show)


data Event = Event
  { eventId :: EventId
  , eventSenderId :: UserId
  , eventReceiverId :: UserId
  , eventPredecessorId :: Maybe EventId
  , eventPayload :: ByteString
  } deriving (Show)


data EventData = EventData
  { event_id :: Required 1 (Value ByteString)
  , event_sender_id :: Required 2 (Value ByteString)
  , event_receiver_id :: Required 3 (Value ByteString)
  , event_predecessor_id :: Optional 4 (Value ByteString)
  , event_payload :: Required 5 (Value ByteString)
  } deriving (Generic, Show)


instance Encode EventData
instance Decode EventData


encodeEvent :: Event -> ByteString
encodeEvent evt = runPut $ encodeMessage $ encodeEventData evt


encodeEventData :: Event -> EventData
encodeEventData Event{..} = EventData
  { event_id = putField $ encodeUUID $ unEventId eventId
  , event_sender_id = putField $ encodeUUID $ unUserId eventSenderId
  , event_receiver_id = putField $ encodeUUID $ unUserId eventReceiverId
  , event_predecessor_id = putField $ case eventPredecessorId of
      Just (EventId uuid) -> Just $ toStrict $ toByteString uuid
      Nothing -> Nothing
  , event_payload = putField eventPayload
  }


encodeUUID :: UUID -> ByteString
encodeUUID = toStrict . toByteString


parseEvent :: ByteString -> Maybe Event
parseEvent bs = do
  case runGet decodeMessage bs of
    Left _ -> Nothing
    Right eventData ->
      parseEventData eventData


parseEventData :: EventData -> Maybe Event
parseEventData eventData = do
  eventId <- EventId <$> (parseUUID $ getField $ event_id eventData)
  eventSenderId <- UserId <$> (parseUUID $ getField $ event_sender_id eventData)
  eventReceiverId <- UserId <$> (parseUUID $ getField $ event_receiver_id eventData)
  eventPredecessorId <- case getField $ event_predecessor_id eventData of
    Just bs -> Just <$> (EventId <$> fromByteString (fromStrict bs))
    Nothing -> Nothing
  let eventPayload = getField $ event_payload eventData
  return Event {..}


parseUUID :: ByteString -> Maybe UUID
parseUUID bs = case fromByteString $ fromStrict bs of
  Just uuid -> Just uuid
  Nothing -> Nothing

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Pushbullet.Types.Ephemeral where

import Network.Pushbullet.Types.Device
import Network.Pushbullet.Types.Misc
import Network.Pushbullet.Types.SMS
import Network.Pushbullet.Types.Time
import Network.Pushbullet.Types.User

import Control.Monad ( guard )
import Data.Aeson
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import Lens.Micro
import Lens.Micro.TH

data Notification
  = Notification
    { _notifThread :: !SmsThreadId
    , _notifTime :: !PushbulletTime
    , _notifTitle :: !Text
    , _notifBody :: !Text
    }
  deriving (Eq, Show)

makeLenses ''Notification

data TickleType
  = PushType
  | OtherType Text
  deriving (Eq, Show)

instance ToJSON TickleType where
  toJSON t = case t of
    PushType -> "push"
    OtherType t' -> toJSON t'

instance FromJSON TickleType where
  parseJSON (String s) = pure $ case s of
    "push" -> PushType
    _ -> OtherType s
  parseJSON _ = fail "cannot parse tickle type from non-string"

data Ephemeral
  = PushEphemeral PushEphemeral
  | Nop
  | Tickle TickleType
  deriving (Eq, Show)


data PushEphemeral
  = Sms
    { _ephSmsSourceUser :: !UserId
    , _ephSmsTargetDevice :: !DeviceId
    , _ephSmsConversation :: !PhoneNumber
    , _ephSmsMessage :: !Text
    }
  | Clipboard
    { _ephClipBody :: !Text
    , _ephClipSourceUser :: !UserId
    , _ephClipSourceDevice :: !DeviceId
    }
  | SmsChanged
    { _ephSourceDevice :: !DeviceId
    , _ephNotifications :: ![Notification]
    }
  deriving (Eq, Show)

makeLenses ''PushEphemeral

instance ToJSON Ephemeral where
  toJSON o = case o of
    Nop -> object
      [ "type" .= id @Text "nop"
      ]
    Tickle subtype -> object
      [ "type" .= id @Text "tickle"
      , "subtype" .= subtype
      ]
    PushEphemeral pushEphemeral -> object
      [ "type" .= id @Text "push"
      , "push" .= pushEphemeral
      ]

instance ToJSON PushEphemeral where
  toJSON o = case o of
    Sms{..} ->  object
      [ "type" .= id @Text "messaging_extension_reply"
      , "package_name" .= id @Text "com.pushbullet.android"
      , "source_user_iden" .= _ephSmsSourceUser
      , "target_device_iden" .= _ephSmsTargetDevice
      , "conversation_iden" .= _ephSmsConversation
      , "message" .= _ephSmsMessage
      ]
    Clipboard{..} -> object
      [ "type" .= id @Text "clip"
      , "body" .= _ephClipBody
      , "source_user_iden" .= _ephClipSourceUser
      , "source_device_iden" .= _ephClipSourceDevice
      ]
    SmsChanged{..} -> object
      [ "type" .= id @Text "sms_changed"
      , "source_device_iden" .= _ephSourceDevice
      , "notifications" .= _ephNotifications
      ]

instance FromJSON Ephemeral where
  parseJSON (Object o) = do
    t <- o .: "type"
    case id @Text t of
      "nop" -> pure Nop
      "tickle" -> Tickle <$> o .: "subtype"
      "push" -> PushEphemeral <$> o .: "push"
      _ -> fail "unknown ephemeral type"

instance FromJSON PushEphemeral where
  parseJSON (Object o) = do
    t' <- o .: "type"
    case t' of
      "messaging_extension_reply" -> pure Sms
        <*> o .: "source_user_iden"
        <*> o .: "target_device_iden"
        <*> o .: "conversation_iden"
        <*> o .: "message"
      "clip" -> pure Clipboard
        <*> o .: "body"
        <*> o .: "source_user_iden"
        <*> o .: "source_device_iden"
      "sms_changed" -> pure SmsChanged
        <*> o .: "source_device_iden"
        <*> o .: "notifications"
      _ -> fail $ "unknown push type " <> t'

instance ToJSON Notification where
  toJSON n = object
    [ "thread_id" .= (n^.notifThread)
    , "timestamp" .= (n^.notifTime)
    , "title" .= (n^.notifTitle)
    , "body" .= (n^.notifBody)
    ]

instance FromJSON Notification where
  parseJSON (Object o) = pure Notification
    <*> o .: "thread_id"
    <*> o .: "timestamp"
    <*> o .: "title"
    <*> o .: "body"
  parseJSON _ = fail "cannot parse notification from non-object"

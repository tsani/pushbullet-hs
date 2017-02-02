{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Pushbullet.Types
( -- * Pagination
  Paginated(..)
, Cursor(..)
  -- * Pushes
, Push(..)
, PushData(..)
, PushTarget(..)
, simpleNewPush
, ExistingPushes(..)
  -- * Ephemerals
, Ephemeral(..)
  -- * Devices
, Device(..)
, DeviceId(..)
, Nickname(..)
, DeviceIcon(..)
, HasSms(..)
, ExistingDevices(..)
, newDevice
  -- * Users
, User(..)
, UserId(..)
  -- * Permanents
, Permanent(..)
, PermanentK(..)
  -- ** SMS
, SmsThreads(..)
, SmsThread(..)
, SmsThreadId(..)
, SmsThreadRecipient(..)
, SmsMessages(..)
, SmsMessage(..)
, SmsId(..)
, SmsDirection(..)
  -- * Misc
, EmailAddress(..)
, ChannelTag(..)
, ClientId(..)
, MimeType(..)
, Url(..)
, Guid(..)
, TrivialObject(..)
, Status(..)
, StatusS(..)
, PushbulletTime(..)
, PhoneNumber(..)
) where

import Control.Applicative ( (<|>) )
import Data.Aeson
import qualified Data.HashMap.Lazy as H
import Data.Monoid ( (<>) )
import Data.Scientific ( toRealFloat )
import Data.String ( IsString(..) )
import Data.Text ( Text )
import Data.Time.Clock ( NominalDiffTime, UTCTime )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds, posixSecondsToUTCTime )
import Web.HttpApiData ( ToHttpApiData(..) )

newtype Cursor = Cursor Text
  deriving (Eq, FromJSON, Show, ToJSON, ToHttpApiData)

data Paginated a = Page !a !(Maybe Cursor)
  deriving (Eq, Show)

instance FromJSON a => FromJSON (Paginated a) where
  parseJSON j@(Object o) = Page <$> parseJSON j <*> o .:? "cursor"
  parseJSON _ = fail "cannot parse paginated data from non-object"

newtype DeviceId = DeviceId Text
  deriving (Eq, FromJSON, Show, ToJSON, ToHttpApiData)

newtype EmailAddress = EmailAddress Text
  deriving (Eq, FromJSON, Show, ToJSON)

newtype ChannelTag = ChannelTag Text
  deriving (Eq, FromJSON, Show, ToJSON)

newtype ChannelId = ChannelId Text
  deriving (Eq, FromJSON, Show, ToJSON)

newtype ClientId = ClientId Text
  deriving (Eq, FromJSON, Show, ToJSON)

newtype MimeType = MimeType Text
  deriving (Eq, FromJSON, Show, ToJSON)

newtype Url = Url Text
  deriving (Eq, FromJSON, Show, ToJSON)

newtype Guid = Guid Text
  deriving (Eq, FromJSON, Show, ToJSON)

newtype PhoneNumber = PhoneNumber Text
  deriving (Eq, FromJSON, Show, ToJSON)

newtype PushId = PushId Text
  deriving (Eq, FromJSON, Show, ToJSON)

data Direction
  = Self
  | Outgoing
  | Incoming
  deriving (Eq, Ord, Show)

instance ToJSON Direction where
  toJSON Self = String "self"
  toJSON Outgoing = String "outgoing"
  toJSON Incoming = String "incoming"

instance FromJSON Direction where
  parseJSON (String s) = case s of
    "self" -> pure Self
    "outgoing" -> pure Outgoing
    "incoming" -> pure Incoming
    _ -> fail "invalid direction string"
  parseJSON _ = fail "cannot parse push direction from non-string"

type family EqT (s :: k) (s' :: k) (a :: *) :: * where
  EqT s s a = a
  EqT _ _ _ = ()

data PushTarget
  = ToAll
  | ToDevice !DeviceId
  | ToEmail !EmailAddress
  | ToChannel !ChannelTag
  | ToClient !ClientId
  deriving (Eq, Show)

data ExistingPushTarget
  = SentBroadcast
  | SentToDevice !DeviceId
  deriving (Eq, Show)

data PushData (s :: Status)
  = NotePush
    { pushTitle :: !Text
    , pushBody :: !Text
    }
  | LinkPush
    { pushTitle :: !Text
    , pushBody :: !Text
    , pushUrl :: !Url
    }
  | FilePush
    { pushBody :: !Text
    , pushFileName :: !Text
    , pushFileType :: !MimeType
    , pushFileUrl :: !Url
    , pushFileTitle :: !(EqT 'Existing s Text)
    , pushImageUrl :: !(EqT 'Existing s (Maybe Url))
    , pushImageWidth :: !(EqT 'Existing s (Maybe Int))
    , pushImageHeight :: !(EqT 'Existing s (Maybe Int))
    }

deriving instance Eq (PushData 'New)
deriving instance Eq (PushData 'Existing)
deriving instance Show (PushData 'New)
deriving instance Show (PushData 'Existing)

data PushOrigin
  = FromClient !ClientId
  | FromChannel !ChannelId
  deriving (Eq, Show)

data Push (s :: Status)
  = Push
    { pushData :: !(PushData s)
    , pushSourceDevice :: !(Maybe DeviceId)
    , pushTarget :: !(EqT 'New s PushTarget)
    , pushGuid :: !(Maybe Guid)
    , pushId :: !(EqT 'Existing s PushId)
    , pushActive :: !(EqT 'Existing s Bool)
    , pushCreated :: !(EqT 'Existing s PushbulletTime)
    , pushModified :: !(EqT 'Existing s PushbulletTime)
    , pushDismissed :: !(EqT 'Existing s Bool)
    , pushDirection :: !(EqT 'Existing s Direction)
    , pushSender :: !(EqT 'Existing s UserId)
    , pushSenderEmail :: !(EqT 'Existing s EmailAddress)
    , pushSenderEmailNormalized :: !(EqT 'Existing s EmailAddress)
    , pushSenderName :: !(EqT 'Existing s Name)
    , pushReceiver :: !(EqT 'Existing s UserId)
    , pushReceiverEmail :: !(EqT 'Existing s EmailAddress)
    , pushReceiverEmailNormalized :: !(EqT 'Existing s EmailAddress)
    , pushExistingTarget :: !(EqT 'Existing s ExistingPushTarget)
    , pushOrigin :: !(EqT 'Existing s (Maybe PushOrigin))
    }

deriving instance Eq (Push 'New)
deriving instance Eq (Push 'Existing)
deriving instance Show (Push 'New)
deriving instance Show (Push 'Existing)

instance FromJSON (Push 'Existing) where
  parseJSON (Object o) = do
    pushType <- text <$> o .: "type"
    d <- case pushType of
      "note" -> pure NotePush
        <*> o .: "title"
        <*> o .: "body"
      "file" -> pure FilePush
        <*> o .: "body"
        <*> o .: "file_name"
        <*> o .: "file_type"
        <*> o .: "file_url"
        <*> o .: "file_title"
        <*> o .:? "image_url"
        <*> o .:? "image_width"
        <*> o .:? "image_height"
      "link" -> pure LinkPush
        <*> o .: "title"
        <*> o .: "body"
        <*> o .: "url"
      _ -> fail "unrecognized push type"
    client <- o .:? "client_iden"
    channel <- o .:? "channel_iden"
    let origin = (FromClient <$> client) <|> (FromChannel <$> channel)

    pure Push
      <*> pure d
      <*> o .:? "source_device_iden"
      <*> pure ()
      <*> o .:? "guid"
      <*> o .: "iden"
      <*> o .: "active"
      <*> o .: "created"
      <*> o .: "modified"
      <*> o .: "dismissed"
      <*> o .: "direction"
      <*> o .: "sender_iden"
      <*> o .: "sender_email"
      <*> o .: "sender_email_normalized"
      <*> o .: "sender_name"
      <*> o .: "receiver_iden"
      <*> o .: "receiver_email"
      <*> o .: "receiver_email_normalized"
      <*> (maybe SentBroadcast SentToDevice <$> o .:? "target_device_iden")
      <*> pure origin

newtype ExistingPushes = ExistingPushes [Push 'Existing]
  deriving (Eq, Show)

instance FromJSON ExistingPushes where
  parseJSON j@(Object o) = ExistingPushes <$> o .: "pushes"

-- | Constructs a new @Push@ with the source device and guid set to @Nothing@.
simpleNewPush :: PushTarget -> PushData 'New -> Push 'New
simpleNewPush t d = Push
  { pushData = d
  , pushSourceDevice = Nothing
  , pushTarget = t
  , pushGuid = Nothing
  , pushId = ()
  , pushActive = ()
  , pushCreated = ()
  , pushModified = ()
  , pushDismissed = ()
  , pushDirection = ()
  , pushSender = ()
  , pushSenderEmail = ()
  , pushSenderEmailNormalized = ()
  , pushSenderName = ()
  , pushReceiver = ()
  , pushReceiverEmail = ()
  , pushReceiverEmailNormalized = ()
  , pushExistingTarget = ()
  , pushOrigin = ()
  }

instance ToJSON (Push 'New) where
  toJSON Push{..} = object (concat pieces) where
    pieces =
      [ [ "source_device_iden" .= pushSourceDevice
        , "guid" .= pushGuid
        ]
      , target
      , thePushData
      ]

    target = case pushTarget of
      ToAll -> []
      ToDevice deviceId -> [ "device_iden" .= deviceId ]
      ToEmail email -> [ "email" .= email ]
      ToChannel tag -> [ "channel_tag" .= tag ]
      ToClient client -> [ "client_iden" .= client ]

    thePushData = case pushData of
      NotePush{..} ->
        [ "type" .= text "note"
        , "title" .= pushTitle
        , "body" .= pushBody
        ]
      LinkPush{..} ->
        [ "type" .= text "link"
        , "title" .= pushTitle
        , "body" .= pushBody
        , "url" .= pushUrl
        ]
      FilePush{..} ->
        [ "type" .= text "file"
        , "body" .= pushBody
        , "file_name" .= pushFileName
        , "file_type" .= pushFileType
        , "file_url" .= pushFileUrl
        ]

text :: Text -> Text
text = id

data Ephemeral
  = Sms
    { ephSmsSourceUser :: !UserId
    , ephSmsTargetDevice :: !DeviceId
    , ephSmsConversation :: !PhoneNumber
    , ephSmsMessage :: !Text
    }
  | Clipboard
    { ephClipBody :: !Text
    , ephClipSourceUser :: !UserId
    , ephClipSourceDevice :: !DeviceId
    }
  deriving (Eq, Show)

instance ToJSON Ephemeral where
  toJSON o = case o of
    Sms{..} -> object
      [ "type" .= text "push"
      , "push" .= object
        [ "type" .= text "messaging_extension_reply"
        , "package_name" .= text "com.pushbullet.android"
        , "source_user_iden" .= ephSmsSourceUser
        , "target_device_iden" .= ephSmsTargetDevice
        , "conversation_iden" .= ephSmsConversation
        , "message" .= ephSmsMessage
        ]
      ]
    Clipboard{..} -> object
      [ "type" .= text "push"
      , "push" .= object
        [ "type" .= text "clip"
        , "body" .= ephClipBody
        , "source_user_iden" .= ephClipSourceUser
        , "source_device_iden" .= ephClipSourceDevice
        ]
      ]

newtype TrivialObject = TrivialObject ()
  deriving (Eq, Ord, Monoid, Show)


instance ToJSON TrivialObject where
  toJSON _ = object []

instance FromJSON TrivialObject where
  parseJSON (Object o)
    | H.null o = pure mempty
    | otherwise = fail "trivial object has no keys"
  parseJSON _ = fail "cannot parse non-object to trivial object"

newtype DeviceIcon = DeviceIcon Text
  deriving (Eq, ToJSON, Show, FromJSON)

newtype Manufacturer = Manufacturer Text
  deriving (Eq, ToJSON, Show, FromJSON)

newtype Fingerprint = Fingerprint Text
  deriving (Eq, ToJSON, Show, FromJSON)

newtype Model = Model Text
  deriving (Eq, ToJSON, Show, FromJSON)

newtype AppVersion = AppVersion Int
  deriving (Eq, ToJSON, Show, FromJSON)

newtype KeyFingerprint = KeyFingerprint Text
  deriving (Eq, ToJSON, Show, FromJSON)

newtype PushToken = PushToken Text
  deriving (Eq, ToJSON, Show, FromJSON)

newtype PushbulletTime = PushbulletTime UTCTime
  deriving (Eq, Show)

instance ToHttpApiData PushbulletTime where
  toUrlPiece (PushbulletTime utc) = fromString (show d) where
    d = fromRational (toRational $ utcTimeToPOSIXSeconds utc)

newtype Nickname = Nickname Text
  deriving (Eq, ToJSON, Show, FromJSON)

data HasSms = NoSms | HasSms
  deriving (Eq, Ord, Show)

instance ToJSON HasSms where
  toJSON NoSms = Bool False
  toJSON HasSms = Bool True

instance FromJSON HasSms where
  parseJSON (Bool True) = pure HasSms
  parseJSON (Bool False) = pure NoSms
  parseJSON _ = fail "cannot parse SMS ability from non-boolean"

instance FromJSON PushbulletTime where
  parseJSON (Number n) = pure (PushbulletTime (posixSecondsToUTCTime p)) where
    p = fromRational (toRational d) :: NominalDiffTime
    d = toRealFloat n :: Double

instance ToJSON PushbulletTime where
  toJSON (PushbulletTime utcTime) = Number $ fromRational (toRational $ utcTimeToPOSIXSeconds utcTime)

data Status
  = New
  | Existing

data StatusS :: Status -> * where
  NewS :: StatusS 'New
  ExistingS :: StatusS 'Existing

data Device (s :: Status)
  = Device
    { deviceId :: !(EqT 'Existing s DeviceId)
    , deviceActive :: !(EqT 'Existing s Bool)
    , deviceCreated :: !(EqT 'Existing s PushbulletTime)
    , deviceModified :: !(EqT 'Existing s PushbulletTime)
    , deviceIcon :: !DeviceIcon
    , deviceNickname :: !(Maybe Nickname)
    , deviceGeneratedNickname :: !(EqT 'Existing s Bool)
    , deviceManufacturer :: !(Maybe Manufacturer)
    , deviceModel :: !(Maybe Model)
    , deviceAppVersion :: !(Maybe AppVersion)
    , deviceFingerprint :: !(EqT 'Existing s (Maybe Fingerprint))
    , deviceKeyFingerprint :: !(EqT 'Existing s (Maybe KeyFingerprint))
    , deviceHasSms :: !HasSms
    , devicePushToken :: !(Maybe PushToken)
    }

deriving instance Eq (Device 'New)
deriving instance Eq (Device 'Existing)
deriving instance Show (Device 'New)
deriving instance Show (Device 'Existing)

newDevice
  :: HasSms
  -> DeviceIcon
  -> Nickname
  -> Maybe Manufacturer
  -> Maybe Model
  -> Maybe AppVersion
  -> Device 'New
newDevice sms icon nick man mod ver = Device
  { deviceId = ()
  , deviceActive = ()
  , deviceCreated = ()
  , deviceModified = ()
  , deviceIcon = icon
  , deviceNickname = Just nick
  , deviceGeneratedNickname = ()
  , deviceManufacturer = man
  , deviceModel = mod
  , deviceAppVersion = ver
  , deviceFingerprint = ()
  , deviceKeyFingerprint = ()
  , deviceHasSms = sms
  , devicePushToken = Nothing
  }

instance ToJSON (Device 'Existing) where
  toJSON Device{..} = object
    [ "iden" .= deviceId
    , "active" .= deviceActive
    , "created" .= deviceCreated
    , "modified" .= deviceModified
    , "icon" .= deviceIcon
    , "nickname" .= deviceNickname
    , "generated_nickname" .= deviceGeneratedNickname
    , "manufacturer" .= deviceManufacturer
    , "model" .= deviceModel
    , "app_version" .= deviceAppVersion
    , "fingerprint" .= deviceFingerprint
    , "key_fingerprint" .= deviceKeyFingerprint
    , "has_sms" .= deviceHasSms
    , "push_token" .= devicePushToken
    ]

instance FromJSON (Device 'Existing) where
  parseJSON (Object o) = pure Device
    <*> o .: "iden"
    <*> o .: "active"
    <*> o .: "created"
    <*> o .: "modified"
    <*> o .: "icon"
    <*> o .:? "nickname"
    <*> o .:? "generated_nickname" .!= False
    <*> o .:? "manufacturer"
    <*> o .:? "model"
    <*> o .:? "app_version"
    <*> o .:? "fingerprint"
    <*> o .:? "key_fingerprint"
    <*> o .:? "has_sms" .!= NoSms
    <*> o .:? "push_token"

instance ToJSON (Device 'New) where
  toJSON Device{..} = object
    [ "nickname" .= deviceNickname
    , "model" .= deviceModel
    , "manufacturer" .= deviceManufacturer
    , "push_token" .= devicePushToken
    , "app_version" .= deviceAppVersion
    , "icon" .= deviceIcon
    , "has_sms" .= deviceHasSms
    ]

newtype ExistingDevices = ExistingDevices [Device 'Existing]
  deriving (Eq, Show)

instance FromJSON ExistingDevices where
  parseJSON (Object o) = ExistingDevices <$> o .: "devices"
  parseJSON _ = fail "cannot parse devices object from non-object"

instance ToJSON ExistingDevices where
  toJSON (ExistingDevices ds) = object [ "devices" .= ds ]

newtype Name = Name Text
  deriving (Eq, FromJSON, Show, ToJSON)

newtype UserId = UserId Text
  deriving (Eq, FromJSON, Show, ToJSON)

data User
  = User
    { userCreated :: PushbulletTime
    , userEmail :: EmailAddress
    , userEmailNormalized :: EmailAddress
    , userId :: UserId
    , userImageUrl :: Url
    , userMaxUploadSize :: Double
    , userModified :: PushbulletTime
    , userName :: Name
    }
  deriving (Eq, Show)

instance FromJSON User where
  parseJSON (Object o) = pure User
    <*> o .: "created"
    <*> o .: "email"
    <*> o .: "email_normalized"
    <*> o .: "iden"
    <*> o .: "image_url"
    <*> o .: "max_upload_size"
    <*> o .: "modified"
    <*> o .: "name"

data SmsDirection
  = IncomingSms
  | OutgoingSms
  deriving (Eq, Show)

instance FromJSON SmsDirection where
  parseJSON (String s) = case s of
    "incoming" -> pure IncomingSms
    "outgoing" -> pure OutgoingSms
    _ -> fail "invalid SMS direction string"
  parseJSON _ = fail "cannot parse SMS direction from non-string"

newtype SmsId = SmsId Text
  deriving (Eq, FromJSON, Show, ToJSON)

data SmsMessageType
  = SMS
  | MMS
  deriving (Eq, Show)

instance FromJSON SmsMessageType where
  parseJSON (String s) = case s of
    "sms" -> pure SMS
    "mms" -> pure MMS
    _ -> fail "invalid SMS type"

data SmsMessage
  = SmsMessage
    { smsDirection :: !SmsDirection
    , smsTime :: !PushbulletTime
    , smsBody :: !Text
    , smsId :: !SmsId
    , smsSent :: !(Maybe Bool)
    , smsType :: !SmsMessageType
    }
  deriving (Eq, Show)

instance FromJSON SmsMessage where
  parseJSON (Object o) = pure SmsMessage
    <*> o .: "direction"
    <*> o .: "time"
    <*> o .: "body"
    <*> o .: "id"
    <*> o .:? "sent"
    <*> o .: "type"

newtype SmsMessages = SmsMessages [SmsMessage]
  deriving (Eq, Show)

instance FromJSON SmsMessages where
  parseJSON (Object o) = SmsMessages <$> o .: "thread"

newtype SmsThreads = SmsThreads [SmsThread]
  deriving (Eq, Show)

instance FromJSON SmsThreads where
  parseJSON (Object o) = SmsThreads <$> o .: "threads"

data SmsThreadRecipient
  = SmsThreadRecipient
    { recipientName :: !Name
    , recipientAddress :: !PhoneNumber
    , recipientNumber :: !PhoneNumber
    }
  deriving (Eq, Show)

instance FromJSON SmsThreadRecipient where
  parseJSON (Object o) = pure SmsThreadRecipient
    <*> o .: "name"
    <*> o .: "address"
    <*> o .: "number"

newtype SmsThreadId = SmsThreadId Text
  deriving (Eq, FromJSON, Show, ToJSON)

data SmsThread
  = SmsThread
    { threadId :: SmsThreadId
    , threadRecipients :: [SmsThreadRecipient]
    , threadLatest :: SmsMessage
    }
  deriving (Eq, Show)

instance FromJSON SmsThread where
  parseJSON (Object o) = pure SmsThread
    <*> o .: "id"
    <*> o .: "recipients"
    <*> o .: "latest"

data PermanentK
  = ThreadList
  | MessageList

data Permanent (p :: PermanentK) where
  ThreadsOf :: !DeviceId -> Permanent 'ThreadList
  MessagesIn :: !DeviceId -> !SmsThreadId -> Permanent 'MessageList

instance ToHttpApiData (Permanent 'ThreadList) where
  toUrlPiece p = case p of
    ThreadsOf (DeviceId d) -> d <> "_threads"

instance ToHttpApiData (Permanent 'MessageList) where
  toUrlPiece p = case p of
    MessagesIn (DeviceId d) (SmsThreadId t) -> d <> "_thread_" <> t

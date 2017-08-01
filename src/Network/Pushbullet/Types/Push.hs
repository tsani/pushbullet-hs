{-# LANGUAGE OverloadedStrings #-}

module Network.Pushbullet.Types.Push
( Push(..)
, PushId(..)
, PushDirection(..)
, PushTarget(..)
, PushData(..)
, PushOrigin(..)
, ExistingPushes(..)
, simpleNewPush
) where

import Network.Pushbullet.Types.Device
import Network.Pushbullet.Types.Misc
import Network.Pushbullet.Types.Status
import Network.Pushbullet.Types.Time
import Network.Pushbullet.Types.User

import Control.Applicative ( (<|>) )
import Data.Aeson
import Data.Text ( Text )

-- | A push. We reuse the same datatype for representing new pushes and
-- existing pushes. The 'EqT' type family is used to enable fields selectively
-- according to whether we're making a new push or representing an existing
-- one.
data Push (s :: Status)
  = Push
    { pushData :: !(PushData s)
    , pushSourceDevice :: !(Maybe DeviceId)
    , pushTarget :: !(PushTarget s)
    , pushGuid :: !(Maybe Guid)
    , pushId :: !(EqT 'Existing s PushId)
    , pushActive :: !(EqT 'Existing s Bool)
    , pushCreated :: !(EqT 'Existing s PushbulletTime)
    , pushModified :: !(EqT 'Existing s PushbulletTime)
    , pushDismissed :: !(EqT 'Existing s Bool)
    , pushDirection :: !(EqT 'Existing s PushDirection)
    , pushSender :: !(EqT 'Existing s UserId)
    , pushSenderEmail :: !(EqT 'Existing s EmailAddress)
    , pushSenderEmailNormalized :: !(EqT 'Existing s EmailAddress)
    , pushSenderName :: !(EqT 'Existing s Name)
    , pushReceiver :: !(EqT 'Existing s UserId)
    , pushReceiverEmail :: !(EqT 'Existing s EmailAddress)
    , pushReceiverEmailNormalized :: !(EqT 'Existing s EmailAddress)
    , pushOrigin :: !(EqT 'Existing s (Maybe PushOrigin))
    }

-- | Unique identifier for a push.
newtype PushId = PushId Text
  deriving (Eq, FromJSON, Show, ToJSON)

-- | The direction of a push.
data PushDirection
  = SelfPush
  | OutgoingPush
  | IncomingPush
  deriving (Eq, Ord, Show)

-- | The target of a push.
data PushTarget (s :: Status) where
  ToAll :: PushTarget 'New
  ToDevice :: !DeviceId -> PushTarget 'New
  ToEmail :: !EmailAddress -> PushTarget 'New
  ToChannel :: !ChannelTag -> PushTarget 'New
  ToClient :: !ClientId -> PushTarget 'New
  SentBroadcast :: PushTarget 'Existing
  SentToDevice :: !DeviceId -> PushTarget 'Existing

-- | The actual contents of a push.
data PushData (s :: Status)
  = NotePush
    { pushTitle :: !(Maybe Text)
    , pushBody :: !Text
    }
  | LinkPush
    { pushTitle :: !(Maybe Text)
    , pushLinkBody :: !(Maybe Text)
    , pushUrl :: !Url
    }
  | FilePush
    { pushFileBody :: !(Maybe Text)
    , pushFileName :: !Text
    , pushFileType :: !MimeType
    , pushFileUrl :: !Url
    , pushFileTitle :: !(EqT 'Existing s (Maybe Text))
    , pushImageUrl :: !(EqT 'Existing s (Maybe Url))
    , pushImageWidth :: !(EqT 'Existing s (Maybe Int))
    , pushImageHeight :: !(EqT 'Existing s (Maybe Int))
    }

-- | The origin of a push.
data PushOrigin
  = FromClient !ClientId
  | FromChannel !ChannelId
  deriving (Eq, Show)

-- | A newtype wrapper for a list of existing pushes. We need this to get a
-- nonstandard 'FromJSON' instance for the list, because Pushbullet gives us
-- the list wrapped in a trivial object with one key.
newtype ExistingPushes
  = ExistingPushes
    { unExistingPushes :: [Push 'Existing]
    }
  deriving (Eq, Show)

-- | Constructs a new @Push@ with the source device and guid set to @Nothing@.
simpleNewPush :: PushTarget 'New -> PushData 'New -> Push 'New
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
  , pushOrigin = ()
  }

instance ToJSON PushDirection where
  toJSON SelfPush = String "self"
  toJSON OutgoingPush = String "outgoing"
  toJSON IncomingPush = String "incoming"

instance FromJSON PushDirection where
  parseJSON (String s) = case s of
    "self" -> pure SelfPush
    "outgoing" -> pure OutgoingPush
    "incoming" -> pure IncomingPush
    _ -> fail "invalid direction string"
  parseJSON _ = fail "cannot parse push direction from non-string"

deriving instance Eq (PushTarget s)
deriving instance Show (PushTarget s)

deriving instance Eq (PushData 'New)
deriving instance Eq (PushData 'Existing)
deriving instance Show (PushData 'New)
deriving instance Show (PushData 'Existing)

deriving instance Eq (Push 'New)
deriving instance Eq (Push 'Existing)
deriving instance Show (Push 'New)
deriving instance Show (Push 'Existing)

instance FromJSON (Push 'Existing) where
  parseJSON (Object o) = do
    pushType <- o .: "type"
    d <- case id @Text pushType of
      "note" -> pure NotePush
        <*> o .:? "title"
        <*> o .: "body"
      "file" -> pure FilePush
        <*> o .:? "body"
        <*> o .: "file_name"
        <*> o .: "file_type"
        <*> o .: "file_url"
        <*> o .:? "file_title"
        <*> o .:? "image_url"
        <*> o .:? "image_width"
        <*> o .:? "image_height"
      "link" -> pure LinkPush
        <*> o .:? "title"
        <*> o .:? "body"
        <*> o .: "url"
      _ -> fail "unrecognized push type"
    client <- o .:? "client_iden"
    channel <- o .:? "channel_iden"
    let origin = (FromClient <$> client) <|> (FromChannel <$> channel)

    pure Push
      <*> pure d
      <*> o .:? "source_device_iden"
      <*> (maybe SentBroadcast SentToDevice <$> o .:? "target_device_iden")
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
      <*> pure origin
  parseJSON _ = fail "cannot parse push from non-object"

instance FromJSON ExistingPushes where
  parseJSON (Object o) = ExistingPushes <$> o .: "pushes"
  parseJSON _ = fail "cannot parse existing pushes from non-object"

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
      ToDevice d -> [ "device_iden" .= d ]
      ToEmail email -> [ "email" .= email ]
      ToChannel tag -> [ "channel_tag" .= tag ]
      ToClient client -> [ "client_iden" .= client ]

    thePushData = case pushData of
      NotePush{..} ->
        [ "type" .= id @Text "note"
        , "title" .= pushTitle
        , "body" .= pushBody
        ]
      LinkPush{..} ->
        [ "type" .= id @Text "link"
        , "title" .= pushTitle
        , "body" .= pushLinkBody
        , "url" .= pushUrl
        ]
      FilePush{..} ->
        [ "type" .= id @Text "file"
        , "body" .= pushFileBody
        , "file_name" .= pushFileName
        , "file_type" .= pushFileType
        , "file_url" .= pushFileUrl
        ]

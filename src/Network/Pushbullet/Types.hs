{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Pushbullet.Types
( -- * Pushes
  Push(..)
, PushData(..)
, PushTarget(..)
, simplePush
  -- * Misc
, DeviceId(..)
, EmailAddress(..)
, ChannelTag(..)
, ClientId(..)
, MimeType(..)
, Url(..)
, Guid(..)
) where

import Data.Aeson
import Data.String ( IsString )
import Data.Text ( Text )

data Cursor a where
  WithCursor :: Text -> a -> Cursor a

newtype DeviceId = DeviceId Text
  deriving (FromJSON, ToJSON)

newtype EmailAddress = EmailAddress Text
  deriving (FromJSON, ToJSON)

newtype ChannelTag = ChannelTag Text
  deriving (FromJSON, ToJSON)

newtype ClientId = ClientId Text
  deriving (FromJSON, ToJSON)

newtype MimeType = MimeType Text
  deriving (FromJSON, ToJSON)

newtype Url = Url Text
  deriving (FromJSON, ToJSON)

newtype Guid = Guid Text
  deriving (FromJSON, ToJSON)

data PushTarget
  = ToAll
  | ToDevice DeviceId
  | ToEmail EmailAddress
  | ToChannel ChannelTag
  | ToClient ClientId

data PushData
  = NotePush
    { pushTitle :: Text
    , pushBody :: Text
    }
  | LinkPush
    { pushTitle :: Text
    , pushBody :: Text
    , pushUrl :: Url
    }
  | FilePush
    { pushBody :: Text
    , pushFileName :: Text
    , pushFileType :: MimeType
    , pushFileUrl :: Url
    }

data Push
  = Push
    { pushData :: PushData
    , pushSourceDevice :: Maybe DeviceId
    , pushTarget :: PushTarget
    , pushGuid :: Maybe Guid
    }

-- | Constructs a @Push@ with the source device and guid set to @Nothing@.
simplePush :: PushTarget -> PushData -> Push
simplePush t d = Push
  { pushData = d
  , pushSourceDevice = Nothing
  , pushTarget = t
  , pushGuid = Nothing
  }

instance ToJSON Push where
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
        , "fiel_url" .= pushFileUrl
        ]

text :: Text -> Text
text = id

{-# LANGUAGE OverloadedStrings #-}

module Network.Pushbullet.Types.Ephemeral where

import Network.Pushbullet.Types.Device
import Network.Pushbullet.Types.Misc
import Network.Pushbullet.Types.User

import Data.Aeson
import Data.Text ( Text )

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
      [ "type" .= id @Text "push"
      , "push" .= object
        [ "type" .= id @Text "messaging_extension_reply"
        , "package_name" .= id @Text "com.pushbullet.android"
        , "source_user_iden" .= ephSmsSourceUser
        , "target_device_iden" .= ephSmsTargetDevice
        , "conversation_iden" .= ephSmsConversation
        , "message" .= ephSmsMessage
        ]
      ]
    Clipboard{..} -> object
      [ "type" .= id @Text "push"
      , "push" .= object
        [ "type" .= id @Text "clip"
        , "body" .= ephClipBody
        , "source_user_iden" .= ephClipSourceUser
        , "source_device_iden" .= ephClipSourceDevice
        ]
      ]


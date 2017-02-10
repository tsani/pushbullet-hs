{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Pushbullet.Types.Ephemeral where

import Network.Pushbullet.Types.Device
import Network.Pushbullet.Types.Misc
import Network.Pushbullet.Types.User

import Data.Aeson
import Data.Text ( Text )
import Lens.Micro.TH

data Ephemeral
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
  deriving (Eq, Show)

makeLenses ''Ephemeral

instance ToJSON Ephemeral where
  toJSON o = case o of
    Sms{..} -> object
      [ "type" .= id @Text "push"
      , "push" .= object
        [ "type" .= id @Text "messaging_extension_reply"
        , "package_name" .= id @Text "com.pushbullet.android"
        , "source_user_iden" .= _ephSmsSourceUser
        , "target_device_iden" .= _ephSmsTargetDevice
        , "conversation_iden" .= _ephSmsConversation
        , "message" .= _ephSmsMessage
        ]
      ]
    Clipboard{..} -> object
      [ "type" .= id @Text "push"
      , "push" .= object
        [ "type" .= id @Text "clip"
        , "body" .= _ephClipBody
        , "source_user_iden" .= _ephClipSourceUser
        , "source_device_iden" .= _ephClipSourceDevice
        ]
      ]

{-# LANGUAGE OverloadedStrings #-}

module Network.Pushbullet.Types.Device where

import Network.Pushbullet.Types.Status
import Network.Pushbullet.Types.Time

import Data.Aeson
import Data.Text ( Text )
import Web.HttpApiData ( ToHttpApiData(..) )

-- | A device attached to a Pushbullet account.
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

-- | Smart constructor for a new device that fills in the ignored fields.
newDevice
  :: HasSms
  -> DeviceIcon
  -> Nickname
  -> Maybe Manufacturer
  -> Maybe Model
  -> Maybe AppVersion
  -> Device 'New
newDevice sms icon nick man m ver = Device
  { deviceId = ()
  , deviceActive = ()
  , deviceCreated = ()
  , deviceModified = ()
  , deviceIcon = icon
  , deviceNickname = Just nick
  , deviceGeneratedNickname = ()
  , deviceManufacturer = man
  , deviceModel = m
  , deviceAppVersion = ver
  , deviceFingerprint = ()
  , deviceKeyFingerprint = ()
  , deviceHasSms = sms
  , devicePushToken = Nothing
  }

-- | A newtype wrapper for a list of existing devices. We need this to get a
-- nonstandard 'FromJSON' instance for the list, because Pushbullet gives us
-- the list wrapped in a trivial object with one key.
newtype ExistingDevices
  = ExistingDevices
    { unExistingDevices :: [Device 'Existing]
    }
  deriving (Eq, Show)

-- | Whether the device has SMS capabilities.
data HasSms = NoSms | HasSms
  deriving (Eq, Ord, Show)

newtype DeviceId = DeviceId Text
  deriving (Eq, FromJSON, Show, ToJSON, ToHttpApiData)

newtype DeviceIcon = DeviceIcon Text
  deriving (Eq, ToJSON, Show, FromJSON)

newtype Nickname = Nickname Text
  deriving (Eq, ToJSON, Show, FromJSON)

newtype PushToken = PushToken Text
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

instance ToJSON HasSms where
  toJSON NoSms = Bool False
  toJSON HasSms = Bool True

instance FromJSON HasSms where
  parseJSON (Bool True) = pure HasSms
  parseJSON (Bool False) = pure NoSms
  parseJSON _ = fail "cannot parse SMS ability from non-boolean"

deriving instance Eq (Device 'New)
deriving instance Eq (Device 'Existing)
deriving instance Show (Device 'New)
deriving instance Show (Device 'Existing)

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
  parseJSON _ = fail "cannot parse existing device from non-object"

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

instance FromJSON ExistingDevices where
  parseJSON (Object o) = ExistingDevices <$> o .: "devices"
  parseJSON _ = fail "cannot parse devices object from non-object"

instance ToJSON ExistingDevices where
  toJSON (ExistingDevices ds) = object [ "devices" .= ds ]

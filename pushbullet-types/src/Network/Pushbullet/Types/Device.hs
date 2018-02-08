{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Pushbullet.Types.Device where

import Network.Pushbullet.Types.Status
import Network.Pushbullet.Types.Time

import Data.Aeson
import Data.Text ( Text )
import Lens.Micro.TH
import Web.HttpApiData ( ToHttpApiData(..) )

newtype DeviceId = DeviceId Text
  deriving (Eq, FromJSON, Show, ToJSON, ToHttpApiData)

-- | An arbitrary string describing what icon to use.
-- Common values, which pushbullet will actually display decent icons for, are
-- provided.
newtype DeviceIcon = DeviceIcon Text
  deriving (Eq, ToJSON, Show, FromJSON)

deviceIconDesktop,
  deviceIconBrowser,
  deviceIconWebsite,
  deviceIconLaptop,
  deviceIconTablet,
  deviceIconPhone,
  deviceIconWatch,
  deviceIconSystem :: DeviceIcon
deviceIconDesktop = DeviceIcon "desktop"
deviceIconBrowser = DeviceIcon "browser"
deviceIconWebsite = DeviceIcon "website"
deviceIconLaptop = DeviceIcon "laptop"
deviceIconTablet = DeviceIcon "tablet"
deviceIconPhone = DeviceIcon "phone"
deviceIconWatch = DeviceIcon "watch"
deviceIconSystem = DeviceIcon "system"

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

-- | Whether the device has SMS capabilities.
data HasSms = NoSms | HasSms
  deriving (Eq, Ord, Show)

-- | A device attached to a Pushbullet account.
data Device (s :: Status)
  = Device
    { _deviceId :: !(EqT 'Existing s DeviceId)
    , _deviceActive :: !(EqT 'Existing s Bool)
    , _deviceCreated :: !(EqT 'Existing s PushbulletTime)
    , _deviceModified :: !(EqT 'Existing s PushbulletTime)
    , _deviceIcon :: !DeviceIcon
    , _deviceNickname :: !(Maybe Nickname)
    , _deviceGeneratedNickname :: !(EqT 'Existing s Bool)
    , _deviceManufacturer :: !(Maybe Manufacturer)
    , _deviceModel :: !(Maybe Model)
    , _deviceAppVersion :: !(Maybe AppVersion)
    , _deviceFingerprint :: !(EqT 'Existing s (Maybe Fingerprint))
    , _deviceKeyFingerprint :: !(EqT 'Existing s (Maybe KeyFingerprint))
    , _deviceHasSms :: !HasSms
    , _devicePushToken :: !(Maybe PushToken)
    }

makeLenses ''Device

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
  { _deviceId = ()
  , _deviceActive = ()
  , _deviceCreated = ()
  , _deviceModified = ()
  , _deviceIcon = icon
  , _deviceNickname = Just nick
  , _deviceGeneratedNickname = ()
  , _deviceManufacturer = man
  , _deviceModel = m
  , _deviceAppVersion = ver
  , _deviceFingerprint = ()
  , _deviceKeyFingerprint = ()
  , _deviceHasSms = sms
  , _devicePushToken = Nothing
  }

-- | A newtype wrapper for a list of existing devices. We need this to get a
-- nonstandard 'FromJSON' instance for the list, because Pushbullet gives us
-- the list wrapped in a trivial object with one key.
newtype ExistingDevices
  = ExistingDevices
    { unExistingDevices :: [Device 'Existing]
    }
  deriving (Eq, Show)

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
    [ "iden" .= _deviceId
    , "active" .= _deviceActive
    , "created" .= _deviceCreated
    , "modified" .= _deviceModified
    , "icon" .= _deviceIcon
    , "nickname" .= _deviceNickname
    , "generated_nickname" .= _deviceGeneratedNickname
    , "manufacturer" .= _deviceManufacturer
    , "model" .= _deviceModel
    , "app_version" .= _deviceAppVersion
    , "fingerprint" .= _deviceFingerprint
    , "key_fingerprint" .= _deviceKeyFingerprint
    , "has_sms" .= _deviceHasSms
    , "push_token" .= _devicePushToken
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
    [ "nickname" .= _deviceNickname
    , "model" .= _deviceModel
    , "manufacturer" .= _deviceManufacturer
    , "push_token" .= _devicePushToken
    , "app_version" .= _deviceAppVersion
    , "icon" .= _deviceIcon
    , "has_sms" .= _deviceHasSms
    ]

instance FromJSON ExistingDevices where
  parseJSON (Object o) = ExistingDevices <$> o .: "devices"
  parseJSON _ = fail "cannot parse devices object from non-object"

instance ToJSON ExistingDevices where
  toJSON (ExistingDevices ds) = object [ "devices" .= ds ]

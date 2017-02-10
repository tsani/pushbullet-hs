{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Pushbullet.Types.User where

import Network.Pushbullet.Types.Misc
import Network.Pushbullet.Types.Time

import Data.Aeson
import Data.Text ( Text )
import Lens.Micro.TH

newtype UserId = UserId Text
  deriving (Eq, FromJSON, Show, ToJSON)

data User
  = User
    { _userCreated :: PushbulletTime
    , _userEmail :: EmailAddress
    , _userEmailNormalized :: EmailAddress
    , _userId :: UserId
    , _userImageUrl :: Url
    , _userMaxUploadSize :: Double
    , _userModified :: PushbulletTime
    , _userName :: Name
    }
  deriving (Eq, Show)

makeLenses ''User

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
  parseJSON _ = fail "cannot parse user from non-object"

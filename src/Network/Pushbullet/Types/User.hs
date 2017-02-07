{-# LANGUAGE OverloadedStrings #-}

module Network.Pushbullet.Types.User where

import Network.Pushbullet.Types.Misc
import Network.Pushbullet.Types.Time

import Data.Aeson
import Data.Text ( Text )

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
  parseJSON _ = fail "cannot parse user from non-object"

newtype UserId = UserId Text
  deriving (Eq, FromJSON, Show, ToJSON)

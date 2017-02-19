module Network.Pushbullet.Types.Misc where

import Data.Aeson
import qualified Data.HashMap.Lazy as H
import Data.Text ( Text )

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

newtype TrivialObject = TrivialObject ()
  deriving (Eq, Ord, Monoid, Show)

newtype Name = Name
  { unName :: Text
  }
  deriving (Eq, FromJSON, Show, ToJSON)

instance ToJSON TrivialObject where
  toJSON _ = object []

instance FromJSON TrivialObject where
  parseJSON (Object o)
    | H.null o = pure mempty
    | otherwise = fail "trivial object has no keys"
  parseJSON _ = fail "cannot parse non-object to trivial object"

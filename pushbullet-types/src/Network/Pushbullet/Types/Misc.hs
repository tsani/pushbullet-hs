module Network.Pushbullet.Types.Misc
( EmailAddress(..),
  ChannelTag(..),
  ChannelId(..),
  ClientId(..),
  MimeType(..),
  Url(..),
  Guid(..),
  PhoneNumber(..),
  TrivialObject,
  trivialObject,
  Name(..)
)
where

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

newtype Url = Url { unUrl :: Text }
  deriving (Eq, FromJSON, Show, ToJSON)

newtype Guid = Guid Text
  deriving (Eq, FromJSON, Show, ToJSON)

newtype PhoneNumber = PhoneNumber Text
  deriving (Eq, FromJSON, Show, ToJSON)

newtype TrivialObject = TrivialObject ()
  deriving (Eq, Ord, Show)

-- | Constructs a trivial object, with no keys.
trivialObject :: TrivialObject
trivialObject = TrivialObject ()

newtype Name = Name
  { unName :: Text
  }
  deriving (Eq, FromJSON, Show, ToJSON)

instance ToJSON TrivialObject where
  toJSON _ = object []

instance FromJSON TrivialObject where
  parseJSON (Object o)
    | H.null o = pure trivialObject
    | otherwise = fail "cannot parse non-trivial object to trivial object"
  parseJSON _ = fail "cannot parse non-object to trivial object"

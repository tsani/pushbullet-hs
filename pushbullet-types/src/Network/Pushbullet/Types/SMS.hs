{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Pushbullet.Types.SMS where

import Data.Aeson
import Data.Text ( Text )
import Data.List.NonEmpty ( NonEmpty )
import Lens.Micro.TH

import Network.Pushbullet.Types.Misc
import Network.Pushbullet.Types.Time

data SmsDirection
  = IncomingSms
  | OutgoingSms
  deriving (Eq, Show)

newtype SmsId = SmsId Text
  deriving (Eq, FromJSON, Show, ToJSON)

data SmsMessageType
  = SMS
  | MMS
  deriving (Eq, Show)

data SmsMessage
  = SmsMessage
    { _smsDirection :: !SmsDirection
    , _smsTime :: !PushbulletTime
    , _smsBody :: !Text
    , _smsId :: !SmsId
    , _smsSent :: !(Maybe Bool)
    , _smsType :: !SmsMessageType
    }
  deriving (Eq, Show)

makeLenses ''SmsMessage

newtype SmsMessages
  = SmsMessages
    { unSmsMessages :: [SmsMessage]
    }
  deriving (Eq, Show)

data SmsThreadRecipient
  = SmsThreadRecipient
    { _recipientName :: !Name
    , _recipientAddress :: !PhoneNumber
    , _recipientNumber :: !PhoneNumber
    }
  deriving (Eq, Show)

makeLenses ''SmsThreadRecipient

newtype SmsThreadId = SmsThreadId Text
  deriving (Eq, FromJSON, Show, ToJSON)

data SmsThread a
  = SmsThread
    { _threadId :: SmsThreadId
    , _threadRecipients :: NonEmpty SmsThreadRecipient
    , _threadLatest :: a
    }
  deriving (Eq, Foldable, Functor, Show, Traversable)

makeLenses ''SmsThread

-- | An SMS conversaion in which it is unknown whether there is a
-- latest message.
type SmsThread' = SmsThread (Maybe SmsMessage)

-- | An SMS conversation in which it is certain that there is a latest
-- message.
type SmsThreadWithLatest = SmsThread SmsMessage

-- | Decides whether the given 'SmsThread' has a latest message.
checkLatestMessage :: SmsThread' -> Maybe SmsThreadWithLatest
checkLatestMessage = sequenceA

newtype SmsThreads
  = SmsThreads
    { unSmsThreads :: [SmsThread']
    }
  deriving (Eq, Show)

instance FromJSON SmsDirection where
  parseJSON (String s) = case s of
    "incoming" -> pure IncomingSms
    "outgoing" -> pure OutgoingSms
    _ -> fail "invalid SMS direction string"
  parseJSON _ = fail "cannot parse SMS direction from non-string"

instance FromJSON SmsMessageType where
  parseJSON (String s)
    | "sms" <- s = pure SMS
    | "mms" <- s = pure MMS
    | otherwise  = fail "invalid SMS type"
  parseJSON _ = fail "cannot parse SMS type from non-string"

instance FromJSON SmsMessage where
  parseJSON (Object o) = pure SmsMessage
    <*> o .: "direction"
    <*> o .: "timestamp"
    <*> o .: "body"
    <*> o .: "id"
    <*> o .:? "sent"
    <*> o .: "type"
  parseJSON _ = fail "cannot parse sms message from non-object"

instance FromJSON SmsMessages where
  parseJSON (Object o) = SmsMessages <$> o .: "thread"
  parseJSON _ = fail "cannot parse sms messages from non-object"

instance FromJSON SmsThreads where
  parseJSON (Object o) = SmsThreads <$> o .: "threads"
  parseJSON _ = fail "cannot parse sms threads from non-object"

instance FromJSON SmsThreadRecipient where
  parseJSON (Object o) = pure SmsThreadRecipient
    <*> o .: "name"
    <*> o .: "address"
    <*> o .: "number"
  parseJSON _ = fail "cannot parse sms thread recipient from non-object"

instance FromJSON a => FromJSON (SmsThread (Maybe a)) where
  parseJSON (Object o) = pure SmsThread
    <*> o .: "id"
    <*> o .: "recipients"
    <*> o .:? "latest"
  parseJSON _ = fail "cannot parse sms thread from non-object"

{-# LANGUAGE OverloadedStrings #-}

module Network.Pushbullet.Types.SMS where

import Data.Aeson
import Data.Text ( Text )
import Data.List.NonEmpty ( NonEmpty )

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
    { smsDirection :: !SmsDirection
    , smsTime :: !PushbulletTime
    , smsBody :: !Text
    , smsId :: !SmsId
    , smsSent :: !(Maybe Bool)
    , smsType :: !SmsMessageType
    }
  deriving (Eq, Show)

newtype SmsMessages
  = SmsMessages
    { unSmsMessages :: [SmsMessage]
    }
  deriving (Eq, Show)

newtype SmsThreads
  = SmsThreads
    { unSmsThreads :: [SmsThread]
    }
  deriving (Eq, Show)

data SmsThreadRecipient
  = SmsThreadRecipient
    { recipientName :: !Name
    , recipientAddress :: !PhoneNumber
    , recipientNumber :: !PhoneNumber
    }
  deriving (Eq, Show)

newtype SmsThreadId = SmsThreadId Text
  deriving (Eq, FromJSON, Show, ToJSON)

data SmsThread
  = SmsThread
    { threadId :: SmsThreadId
    , threadRecipients :: NonEmpty SmsThreadRecipient
    , threadLatest :: SmsMessage
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

instance FromJSON SmsThread where
  parseJSON (Object o) = pure SmsThread
    <*> o .: "id"
    <*> o .: "recipients"
    <*> o .: "latest"
  parseJSON _ = fail "cannot parse sms thread from non-object"

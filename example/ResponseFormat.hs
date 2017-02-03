{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module ResponseFormat where

import Format
import Response

import Network.Pushbullet.Types

import Data.Aeson ( ToJSON(..), (.=), Value(Number), object )
import Data.List ( sortBy )
import Data.Ord ( comparing )
import qualified Data.Text as T
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( getTimeZone, utcToLocalTime )
import qualified Text.PrettyPrint.ANSI.Leijen as P

formatJsv :: Monad m => FormatM ResponseInfo m JSV
formatJsv = FormatM $ \r -> pure $ JSV $ case r of
  SmsList msgs -> (pure . JsvCell . Formatted) <$> msgs
  ThreadList threads -> (pure . JsvCell . Formatted) <$> threads
  DeviceList devices -> (pure . JsvCell . Formatted) <$> devices
  Ok -> [[JsvCell @T.Text "ok"]]

formatHumanTable :: FormatM ResponseInfo IO HumanTable
formatHumanTable = FormatM $ \case
  SmsList (chronological -> msgs) ->
    HumanTable . P.vcat <$> (mapM phi msgs)
  where
    chronological = sortBy (comparing smsTime)

    phi :: SmsMessage -> IO P.Doc
    phi SmsMessage{..} = do
      t <- niceTime smsTime
      pure $
        P.hang 4 (
          P.group $
            (P.text t P.<+> arrow smsDirection) P.<$>
            P.fillSep (map P.text $ words $ T.unpack smsBody)
        )

    d = defaultTimeLocale
    niceTime (PushbulletTime t) =
      formatTime d "%a %d %b %Y @ %H:%M:%S"
        <$> (utcToLocalTime <$> getTimeZone t <*> pure t)

    arrow dir = case dir of
      OutgoingSms -> ">"
      IncomingSms -> "<"

-- | A simple newtype wrapper so that we can special ToJSON instances for the
-- output.
newtype Formatted a = Formatted a

instance ToJSON (Formatted PushbulletTime) where
  toJSON (Formatted (PushbulletTime t)) = Number d where
    d = fromRational (toRational $ utcTimeToPOSIXSeconds t)

instance ToJSON (Formatted SmsMessage) where
  toJSON (Formatted SmsMessage{..}) = object
    [ "direction" .= id @T.Text (
      case smsDirection of
        IncomingSms -> "incoming"
        OutgoingSms -> "outgoing"
      )
    , "time" .= Formatted smsTime
    , "body" .= smsBody
    , "smsId" .= smsId
    , "smsType" .= id @T.Text (
      case smsType of
        SMS -> "sms"
        MMS -> "mms"
      )
    ]

instance ToJSON (Formatted SmsThread) where
  toJSON (Formatted SmsThread{..}) = object
    [ "id" .= threadId
    , "recipients" .= (Formatted <$> threadRecipients)
    , "latest" .= Formatted threadLatest
    ]

instance ToJSON (Formatted SmsThreadRecipient) where
  toJSON (Formatted SmsThreadRecipient{..}) = object
    [ "name" .= recipientName
    , "number" .= recipientNumber
    ]

instance ToJSON (Formatted (Device 'Existing)) where
  toJSON (Formatted Device{..}) = object
    [ "id" .= deviceId
    , "active" .= deviceActive
    , "name" .= deviceNickname
    , "hasSms" .= deviceHasSms
    , "manufacturer" .= deviceManufacturer
    , "model" .= deviceModel
    ]

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module ResponseFormat.HumanTable where

import Format
import Response

import Network.Pushbullet.Types

import Data.Function ( on )
import Data.List ( groupBy, sortBy )
import Data.Ord ( comparing )
import qualified Data.Text as T
import Data.Time.Clock ( diffUTCTime, NominalDiffTime )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( getTimeZone, utcToLocalTime )
import qualified Text.PrettyPrint.ANSI.Leijen as P

newtype HumanTable = HumanTable P.Doc
  deriving RenderableFormat

formatHumanTable :: FormatM ResponseInfo IO HumanTable
formatHumanTable = FormatM $ \case
  SmsList (chronological -> groupSms d -> groups) ->
    HumanTable . P.vcat <$> (mapM phi groups)
  where
    chronological = sortBy (comparing smsTime)

    phi :: SmsGroup -> IO P.Doc
    phi (SmsGroup dir msgs) = do
      let msg = head msgs
      t <- niceTime (smsTime msg)
      pure $
        P.text t P.<+> arrow dir P.<> ( P.nest 2 . (P.line P.<>) $
          P.vsep $ (<$> msgs) (\m -> P.hang 2 $
            P.fillSep (map P.text $ words $ T.unpack (smsBody m))
          )
        ) P.<$> P.empty

    niceTime (PushbulletTime t) =
      formatTime defaultTimeLocale "%a %d %b %Y @ %H:%M:%S"
        <$> (utcToLocalTime <$> getTimeZone t <*> pure t)

    arrow dir = case dir of
      OutgoingSms -> ">"
      IncomingSms -> "<"

    d :: NominalDiffTime
    d = 60 * 10 -- ten minutes

-- | An SMS group is a bunch of messages from a given person ordered
-- chronologically such that the timestamps of adjacent messages differ by no
-- more than a fixed amount.
data SmsGroup
  = SmsGroup !SmsDirection ![SmsMessage]

-- | Given a list of SMS ordered chronologically and a maximum time difference,
-- group the messages.
groupSms :: NominalDiffTime -> [SmsMessage] -> [SmsGroup]
groupSms d msgs = concat (fmap (fmap groupup) timeframe) where
  groupup :: [SmsMessage] -> SmsGroup
  groupup xs@(x:_) = SmsGroup (smsDirection x) xs

  timeframe :: [[[SmsMessage]]]
  timeframe = groupBy checkTime <$> samedir

  samedir :: [[SmsMessage]]
  samedir = groupBy ((==) `on` smsDirection) msgs

  checkTime
    (SmsMessage{smsTime=PushbulletTime u1})
    (SmsMessage{smsTime=PushbulletTime u2}) = diffUTCTime u2 u1 < d

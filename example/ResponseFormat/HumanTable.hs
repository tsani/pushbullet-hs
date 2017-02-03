{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module ResponseFormat.HumanTable where

import Format
import Response

import Network.Pushbullet.Types

import Data.List ( sortBy )
import Data.Ord ( comparing )
import qualified Data.Text as T
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( getTimeZone, utcToLocalTime )
import qualified Text.PrettyPrint.ANSI.Leijen as P

newtype HumanTable = HumanTable P.Doc
  deriving RenderableFormat

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

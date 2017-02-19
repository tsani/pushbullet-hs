{-# LANGUAGE OverloadedStrings #-}

module Network.Pushbullet.Types.Permanent where

import Network.Pushbullet.Types.Device
import Network.Pushbullet.Types.SMS

import Data.Monoid ( (<>) )
import Web.HttpApiData ( ToHttpApiData(..) )

data PermanentK
  = ThreadList
  | MessageList

data Permanent (p :: PermanentK) where
  ThreadsOf :: !DeviceId -> Permanent 'ThreadList
  MessagesIn :: !DeviceId -> !SmsThreadId -> Permanent 'MessageList

instance ToHttpApiData (Permanent 'ThreadList) where
  toUrlPiece p = case p of
    ThreadsOf (DeviceId d) -> d <> "_threads"

instance ToHttpApiData (Permanent 'MessageList) where
  toUrlPiece p = case p of
    MessagesIn (DeviceId d) (SmsThreadId t) -> d <> "_thread_" <> t

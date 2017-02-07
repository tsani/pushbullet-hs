{-# LANGUAGE OverloadedStrings #-}

module Network.Pushbullet.Types.Permanent where

import Network.Pushbullet.Types.Device
import Network.Pushbullet.Types.SMS

import Data.Monoid ( (<>) )
import Web.HttpApiData ( ToHttpApiData(..) )

data PermanentK
  = ThreadListK
  | MessageListK

data Permanent (p :: PermanentK) where
  ThreadsOf :: !DeviceId -> Permanent 'ThreadListK
  MessagesIn :: !DeviceId -> !SmsThreadId -> Permanent 'MessageListK

instance ToHttpApiData (Permanent 'ThreadListK) where
  toUrlPiece p = case p of
    ThreadsOf (DeviceId d) -> d <> "_threads"

instance ToHttpApiData (Permanent 'MessageListK) where
  toUrlPiece p = case p of
    MessagesIn (DeviceId d) (SmsThreadId t) -> d <> "_thread_" <> t

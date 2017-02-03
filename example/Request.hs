{-# LANGUAGE GADTs #-}

module Request where

import Format

import Network.Pushbullet.Types

import qualified Data.Text as T

data Request m res key dev count where
  Request
    :: ExistsRenderableFormat (FormatM res m)
    -> key
    -> RequestInfo dev count
    -> Request m res key dev count

data RequestInfo dev count
  = ListSmsReq dev SmsThreadId
  | ListThreadsReq dev
  | SendSmsReq dev PhoneNumber T.Text
  | ListDevicesReq count

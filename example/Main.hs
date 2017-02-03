{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Count
import Command
import Format
import Request
import Response
import ResponseFormat

import Network.Pushbullet.Api
import Network.Pushbullet.Client
import Network.Pushbullet.Types

import Control.Monad.Free
import Data.Bifunctor
import Data.ByteString ( readFile )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8 )
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Prelude hiding ( readFile )
import Options.Applicative
import Servant.Client
import System.Directory ( getHomeDirectory )
import System.FilePath ( (</>) )
import System.IO ( stderr, hPutStrLn )

main :: IO ()
main
  = execParser opts -- parse commandline options
  >>= loadDefaults  -- load any defaults to fill in the gaps in the options
  >>= run           -- execute the request
  >>= output        -- pretty-print the response

output :: Either Error Response' -> IO ()
output e = case e of
  Left err -> ePutStrLn $ "an error occurred: " ++ show err
  Right (Response (ExistsRenderableFormat (FormatM format)) res) ->
    LBS.putStr . renderFormat =<< format res

fullDescInfo :: Parser a -> ParserInfo a
fullDescInfo p = info (helper <*> p) fullDesc

opts :: ParserInfo PreRequest
opts = fullDescInfo cliRequest

raw :: ReadM T.Text
raw = T.pack <$> str

cliRequest :: Parser PreRequest
cliRequest
  = pure Request
  <*> (
    flag
      (ExistsRenderableFormat formatHumanTable)
      (ExistsRenderableFormat formatJsv)
      (long "jsv")
  )
  <*> optional (option (PushbulletKey <$> raw) (long "key"))
  <*> subparser cliRequestInfo

cliRequestInfo :: Mod CommandFields (RequestInfo (Maybe DeviceId) (Maybe Count))
cliRequestInfo
  = command "sms" (fullDescInfo $ subparser cliSms)
  <> command "devices" (fullDescInfo $ subparser cliDevices)

cliSms :: Mod CommandFields (RequestInfo (Maybe DeviceId) count)
cliSms
  = command "list" (
    fullDescInfo $
      pure ListSmsReq
        <*> optional (option (DeviceId <$> raw) (long "device"))
        <*> option (SmsThreadId <$> raw) (long "thread")
  )
  <> command "send" (
    fullDescInfo $
      pure SendSmsReq
        <*> optional (option (DeviceId <$> raw) (long "device"))
        <*> option (PhoneNumber <$> raw) (long "dest")
        <*> option raw (long "message")
  )
  <> command "threads" (
    fullDescInfo $
      pure ListThreadsReq
        <*> optional (option (DeviceId <$> raw) (long "device"))
  )

cliDevices :: Mod CommandFields (RequestInfo dev (Maybe Count))
cliDevices
  = command "list" $ fullDescInfo (
    pure ListDevicesReq
      <*> optional (
        flag' All (long "all")
        <|>
        option (Limit <$> auto) (long "limit")
      )
  )

type PreRequest
  = Request IO ResponseInfo (Maybe PushbulletKey) (Maybe DeviceId) (Maybe Count)
type PreparedRequest
  = Request IO ResponseInfo PushbulletKey DeviceId Count

-- | Interprets a computation in the 'Command' monad into a computation in the
-- 'ClientM' monad that actually performs HTTP requests.
httpCommand :: PushbulletKey -> Command a -> ClientM a
httpCommand key = iterM phi where
  auth = pushbulletAuth key

  phi :: CommandF (ClientM a) -> ClientM a
  phi com = case com of
    ListSms d t k ->
      k . unSmsMessages =<< getSmsMessages auth (d `MessagesIn` t)
    ListThreads d k ->
      k . unSmsThreads =<< getSmsThreads auth (ThreadsOf d)
    SendSms u d n m k -> createEphemeral auth (Sms u d n m) *> k
    ListDevices count k -> do
      let getDevices' = fmap (fmap unExistingDevices) . getDevices auth
      start <- getDevices' Nothing
      k =<< getPaginatedLimit count start (getDevices' . Just)
    Me k -> k =<< getMe auth

getPaginatedLimit
  :: Monad m
  => Count
  -> Paginated [a]
  -> (Cursor -> m (Paginated [a]))
  -> m [a]
getPaginatedLimit All (Page d Nothing) _
  = pure d
getPaginatedLimit All (Page d (Just c)) next = do
  p <- next c
  later <- getPaginatedLimit All p next
  pure (d ++ later)
getPaginatedLimit (Limit n) _ _ | n <= 0
  = pure []
getPaginatedLimit (Limit n) (Page d Nothing) _
  = pure (take n d)
getPaginatedLimit (Limit n) (Page d (Just c)) next = do
  let d' = take n d
  let n' = n - length d'
  p <- next c
  later <- getPaginatedLimit (Limit n') p next
  pure (d' ++ later)

loadDefaults
  :: PreRequest
  -> IO PreparedRequest
loadDefaults (Request fmt key inf) = Request fmt <$> key' <*> inf' where
  loadLine p = decodeUtf8 . BS.init <$> readFile p

  key' = maybe (PushbulletKey <$> (loadLine =<< keyPath)) pure key where
    keyPath = (</> ".pushbulletaccess") <$> getHomeDirectory

  inf' = case inf of
    ListSmsReq d t -> ListSmsReq <$> maybeLoadDeviceId d <*> pure t
    ListThreadsReq d -> ListThreadsReq <$> maybeLoadDeviceId d
    SendSmsReq d n t -> SendSmsReq <$> maybeLoadDeviceId d <*> pure n <*> pure t
    ListDevicesReq c -> pure $ ListDevicesReq (maybe All id c)
    where
      maybeLoadDeviceId = maybe loadDeviceId pure where
        loadDeviceId = DeviceId <$> (loadLine =<< deviceIdPath) where
          deviceIdPath = (</> ".phoneDeviceID") <$> getHomeDirectory

data Error
  = ServantError ServantError
  deriving (Eq, Show)

type Response' = Response (ExistsRenderableFormat (FormatM ResponseInfo IO))

run :: PreparedRequest -> IO (Either Error Response')
run (Request format key req) = do
  manager <- newManager tlsManagerSettings
  let url = BaseUrl Https "api.pushbullet.com" 443 ""
  -- let url = BaseUrl Http "localhost" 8088 ""
  let env = ClientEnv manager url

  let runHttp = fmap (first ServantError) . flip runClientM env
        :: forall c. ClientM c -> IO (Either Error c)

  case req of
    ListSmsReq d t -> do
      r <- runHttp (httpCommand key $ listSms d t)
      pure $ Response format <$> (SmsList <$> r)
    ListThreadsReq d -> do
      r <- runHttp (httpCommand key $ listThreads d)
      pure $ Response format <$> (ThreadList <$> r)
    ListDevicesReq count -> do
      r <- runHttp (httpCommand key $ listDevices count)
      pure $ Response format <$> (DeviceList <$> r)
    SendSmsReq d n t -> do
      r <- fmap (fmap (const Ok)) $ runHttp $ httpCommand key $ do
        User {userId = i} <- me
        sendSms i d n t
      pure $ Response format <$> r

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

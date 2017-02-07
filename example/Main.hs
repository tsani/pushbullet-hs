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
import Sum

import Network.Pushbullet.Api
import Network.Pushbullet.Client
import Network.Pushbullet.Types

import Control.Monad.Free
import Data.Bifunctor ( first )
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
import System.IO ( stderr, hPutStrLn )

main :: IO ()
main
  = execParser opts    -- parse commandline options
  >>= bleh
  >>= run              -- execute the request
  -- >>= output   -- pretty-print the response

fullDescInfo :: Parser a -> ParserInfo a
fullDescInfo p = info (helper <*> p) fullDesc

bleh :: Request IO (IO PushbulletKey) -> IO (Request IO PushbulletKey)
bleh = sequence

opts :: ParserInfo (Request IO (IO PushbulletKey))
opts = fullDescInfo cliRequest

raw :: ReadM T.Text
raw = T.pack <$> str

line :: FilePath -> IO T.Text
line p = decodeUtf8 . BS.init <$> readFile p

cliRequest :: Parser (Request IO (IO PushbulletKey))
cliRequest
  = pure Request
  <*> flag
    (ExistsRenderableFormat <$> formatHumanTable)
    (ExistsRenderableFormat . pure <$> formatJsv)
    (long "jsv")
  <*> option
    (fmap pure $ PushbulletKey <$> raw)
    (long "key" <> value (PushbulletKey <$> line access))
  <*> subparser (
    command "sms" (
      fullDescInfo $ subparser (
        command "list" (
          fullDescInfo $
            pure (\d t -> do
              d' <- case d of
                Nothing -> DeviceId <$> line device
                Just x -> pure x
              pure $ inject <$> listSms d' t
            )
            <*> optional (option (DeviceId <$> raw) (long "device"))
            <*> option (SmsThreadId <$> raw) (long "thread")
        )
        <>
        command "send" (
          fullDescInfo $
            pure (\d n m -> do
              d' <- maybe (fmap DeviceId $ line device) pure d
              pure $ inject <$> do
                User {userId = u} <- me
                sendSms u d' n m
            )
            <*> optional (option (DeviceId <$> raw) (long "device"))
            <*> option (PhoneNumber <$> raw) (long "number")
            <*> option raw (long "message")
        )
        <>
        command "threads" (
          fullDescInfo $
            pure (\d -> do
              d' <- maybe (fmap DeviceId $ line device) pure d
              pure $ inject <$> listThreads d'
            )
            <*> optional (option (DeviceId <$> raw) (long "device"))
        )
      )
    )
    <>
    command "devices" (
      fullDescInfo $ subparser (
        command "list" (
          fullDescInfo $
            pure (\c -> do
              let c' = maybe All id c
              pure $ inject <$> listDevices c'
            )
            <*> optional (option (Limit <$> auto) (long "limit"))
        )
      )
    )
  )

device :: String
device = "/home/tsani/.phoneDeviceID"

access :: String
access = "/home/tsani/.pushbulletaccess"

-- cliSms :: Mod CommandFields (RequestInfo (Maybe DeviceId) count)
-- cliSms
--   = command "list" (
--     fullDescInfo $
--       pure ListSmsReq
--         <*> option
--           (fmap pure (DeviceId <$> raw))
--           (long "device")
--         <*> option
--           (SmsThreadId <$> raw)
--           (long "thread")
--   )
--   <> command "send" (
--     fullDescInfo $
--       pure SendSmsReq
--         <*> optional (option (DeviceId <$> raw) (long "device"))
--         <*> option (PhoneNumber <$> raw) (long "dest")
--         <*> option raw (long "message")
--   )
--   <> command "threads" (
--     fullDescInfo $
--       pure ListThreadsReq
--         <*> optional (option (DeviceId <$> raw) (long "device"))
--   )
--
-- cliDevices :: Mod CommandFields (RequestInfo dev (Maybe Count))
-- cliDevices
--   = command "list" $ fullDescInfo (
--     pure ListDevicesReq
--       <*> optional (
--         flag' All (long "all")
--         <|>
--         option (Limit <$> auto) (long "limit")
--       )
--   )

type Request'
  = Request IO PushbulletKey

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

data Error
  = ServantError ServantError
  deriving (Eq, Show)

type Response' = Response (ExistsRenderableFormat (FormatM ResponseInfo IO))

run :: Request IO PushbulletKey -> IO ()
run (Request format key cmd) = do
  -- prepare the command
  cmd' <- cmd

  -- get ready to do some motherfucking http requests
  manager <- newManager tlsManagerSettings
  let url = BaseUrl Https "api.pushbullet.com" 443 ""
  -- let url = BaseUrl Http "localhost" 8088 ""
  let env = ClientEnv manager url

  let runHttp = fmap (first ServantError) . flip runClientM env
        :: forall c. ClientM c -> IO (Either Error c)

  r <- runHttp (httpCommand key cmd')

  let q = match' format <$> r
  case q of
    Left e -> ePutStrLn (show e)
    Right (ExistsRenderableFormat f) -> LBS.putStr =<< renderFormat <$> f

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

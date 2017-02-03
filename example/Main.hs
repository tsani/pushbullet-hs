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

import Network.Pushbullet.Api
import Network.Pushbullet.Client
import Network.Pushbullet.Types

import Control.Monad.Free
import Data.Aeson ( encode, ToJSON(..), Value(Number), object, (.=) )
import Data.Bifunctor
import Data.ByteString ( readFile )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List ( sortBy )
import Data.Monoid ( (<>) )
import Data.Ord ( comparing )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( getTimeZone, utcToLocalTime )
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Prelude hiding ( readFile )
import Options.Applicative
import Servant.Client
import System.Directory ( getHomeDirectory )
import System.FilePath ( (</>) )
import System.IO ( stderr, hPutStrLn )
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen hiding ( (<>), (<$>), (</>) )

data Count
  = All
  | Limit Int

data CommandF a where
  ListSms :: DeviceId -> SmsThreadId -> ([SmsMessage] -> a) -> CommandF a
  ListThreads :: DeviceId -> ([SmsThread] -> a) -> CommandF a
  SendSms :: UserId -> DeviceId -> PhoneNumber -> T.Text -> a -> CommandF a
  ListDevices :: Count -> ([Device 'Existing] -> a) -> CommandF a
  Me :: (User -> a) -> CommandF a
  deriving Functor

type Command = Free CommandF

listSms :: DeviceId -> SmsThreadId -> Command [SmsMessage]
listSms d t = liftF (ListSms d t id)

listThreads :: DeviceId -> Command [SmsThread]
listThreads d = liftF (ListThreads d id)

sendSms :: UserId -> DeviceId -> PhoneNumber -> T.Text -> Command ()
sendSms u d p t = liftF (SendSms u d p t ())

listDevices :: Count -> Command [Device 'Existing]
listDevices c = liftF (ListDevices c id)

me :: Command User
me = liftF (Me id)

data SomeRenderableFormatter m
  = forall fmt. RenderableFormat fmt
  => SomeRenderableFormatter
    { getFormatter :: ResponseFormatterM m fmt
    }

data Request m key dev count where
  Request
    :: SomeRenderableFormatter m
    -> key
    -> RequestInfo dev count
    -> Request m key dev count

data RequestInfo dev count
  = ListSmsReq dev SmsThreadId
  | ListThreadsReq dev
  | SendSmsReq dev PhoneNumber T.Text
  | ListDevicesReq count

data Response m where
  Response
    :: SomeRenderableFormatter m
    -> ResponseInfo
    -> Response m

data ResponseInfo
  = SmsList [SmsMessage]
  | ThreadList [SmsThread]
  | DeviceList [Device 'Existing]
  | Ok

type ResponseFormatterM m fmt = ResponseInfo -> m fmt

class RenderableFormat a where
  renderFormat :: a -> LBS.ByteString

instance RenderableFormat LBS.ByteString where
  renderFormat = id

instance RenderableFormat BS.ByteString where
  renderFormat = LBS.fromStrict

instance RenderableFormat Doc where
  renderFormat
    = LBS.fromStrict
    . encodeUtf8
    . T.pack
    . ($ "")
    . displayS
    . renderPretty 0.75 80

instance RenderableFormat T.Text where
  renderFormat = LBS.fromStrict . encodeUtf8

newtype HumanTable = HumanTable Doc
  deriving RenderableFormat

newtype JSV = JSV [[JsvCell]]

data JsvCell where
  JsvCell :: ToJSON a => !a -> JsvCell

instance ToJSON JsvCell where
  toJSON (JsvCell cell) = toJSON cell

instance RenderableFormat JSV where
  renderFormat (JSV rows)
    = LBS.concat ((<> "\n") . LBS.intercalate "," . fmap encode <$> rows)

formatJsv :: Monad m => ResponseFormatterM m JSV
formatJsv r = pure $ JSV $ case r of
  SmsList msgs -> (pure . JsvCell . Formatted) <$> msgs
  ThreadList threads -> (pure . JsvCell . Formatted) <$> threads
  DeviceList devices -> (pure . JsvCell . Formatted) <$> devices
  Ok -> [[JsvCell @T.Text "ok"]]

formatHumanTable :: ResponseFormatterM IO HumanTable
formatHumanTable r = case r of
  SmsList (chronological -> msgs) ->
    HumanTable . vcat <$> (mapM phi msgs)
  where
    chronological = sortBy (comparing smsTime)

    phi :: SmsMessage -> IO Doc
    phi SmsMessage{..} = do
      t <- niceTime smsTime
      pure $
        hang 4 (
          group $
            (text t <+> arrow smsDirection) PP.<$>
            fillSep (map text $ words $ T.unpack smsBody)
        )

    d = defaultTimeLocale
    niceTime (PushbulletTime t) =
      formatTime d "%a %d %b %Y @ %H:%M:%S"
        <$> (utcToLocalTime <$> getTimeZone t <*> pure t)

    arrow dir = case dir of
      OutgoingSms -> ">"
      IncomingSms -> "<"

main :: IO ()
main
  = execParser opts -- parse commandline options
  >>= loadDefaults  -- load any defaults
  >>= run           -- execute the request
  >>= output        -- pretty-print the response
  where
    output :: Either Error (Response IO) -> IO ()
    output e = case e of
      Left err -> ePutStrLn $ "an error occurred: " ++ show err
      Right (Response (SomeRenderableFormatter format) res) ->
        LBS.putStr . renderFormat =<< format res

    fullDescInfo p = info (helper <*> p) fullDesc

    opts = fullDescInfo cliRequest

    raw = T.pack <$> str

    cliRequest :: Parser PreRequest
    cliRequest
      = pure Request
      <*> (
        flag
          (SomeRenderableFormatter formatHumanTable)
          (SomeRenderableFormatter formatJsv)
          (long "jsv")
      )
      <*> optional (option (PushbulletKey <$> raw) (long "key"))
      <*> subparser cliRequestInfo

    cliRequestInfo
      = command "sms" (fullDescInfo $ subparser cliSms)
      <> command "devices" (fullDescInfo $ subparser cliDevices)

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
  = Request IO (Maybe PushbulletKey) (Maybe DeviceId) (Maybe Count)
type PreparedRequest
  = Request IO PushbulletKey DeviceId Count

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

run :: PreparedRequest -> IO (Either Error (Response IO))
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

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

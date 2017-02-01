{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Pushbullet.Api
import Network.Pushbullet.Client
import Network.Pushbullet.Types

import Data.ByteString ( readFile )
import qualified Data.ByteString as BS
import Data.Text.Encoding ( decodeUtf8 )
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Prelude hiding ( readFile )
import Servant.API
import Servant.Client

main :: IO ()
main = do
  authtext <- decodeUtf8 . BS.init <$> readFile "/home/tsani/.pushbulletaccess"
  print authtext
  let auth = pushbulletAuth (PushbulletKey authtext)

  manager <- newManager tlsManagerSettings
  let url = BaseUrl Https "api.pushbullet.com" 443 ""
  let env = ClientEnv manager url

  putStrLn "gonna send push"

  let (createPush :<|> getPushes) :<|> createEphemeral :<|> getMe :<|> (getDevices :<|> createDevice :<|> deleteDevice) = pushbulletApiClient auth

  e <- flip runClientM env $ do
    p <- createPush $ simpleNewPush
      ToAll
      NotePush { pushTitle = "hi", pushBody = "boobs" }

    ds <- getMe

    pure (p, ds)

  case e of
    Left err -> print err
    Right x -> print x

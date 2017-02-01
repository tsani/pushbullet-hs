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
import Servant.Client

createPush = pushbulletApiClient

main :: IO ()
main = do
  authtext <- decodeUtf8 . BS.init <$> readFile "/home/tsani/.pushbulletaccess"
  print authtext
  let auth = pushbulletAuth (PushbulletKey authtext)

  manager <- newManager tlsManagerSettings
  -- let url = BaseUrl Http "localhost" 8088 ""
  let url = BaseUrl Https "api.pushbullet.com" 443 ""
  let env = ClientEnv manager url

  putStrLn "gonna send push"

  e <- flip runClientM env $ do
    createPush auth $ simplePush
      ToAll
      NotePush { pushTitle = "hi", pushBody = "boobs" }

  case e of
    Left err -> print err
    Right x -> print x

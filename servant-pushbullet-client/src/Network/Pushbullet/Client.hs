{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Network.Pushbullet.Client where

import Network.Pushbullet.Api
import Network.Pushbullet.Types

import Servant.Client
import Servant.Common.Req ( addHeader )
import Servant.API hiding ( addHeader )

createPush
  :: Auth
  -> Push 'New
  -> ClientM (Push 'Existing)
getPushes
  :: Auth
  -> Maybe PushbulletTime
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Cursor
  -> ClientM (Paginated ExistingPushes)
getPush
  :: Auth
  -> PushId
  -> ClientM (Push 'Existing)
deletePush
  :: Auth
  -> PushId
  -> ClientM TrivialObject
createEphemeral :: Auth -> Ephemeral -> ClientM TrivialObject
getMe :: Auth -> ClientM User
getDevices
  :: Auth
  -> Maybe Bool
  -> Maybe Cursor
  -> ClientM (Paginated ExistingDevices)
createDevice
  :: Auth
  -> Device 'New
  -> ClientM (Device 'Existing)
deleteDevice
  :: Auth
  -> DeviceId
  -> ClientM TrivialObject
getSmsThreads
  :: Auth
  -> Permanent 'ThreadList
  -> ClientM SmsThreads
getSmsMessages
  :: Auth
  -> Permanent 'MessageList
  -> ClientM SmsMessages
(createPush :<|> getPushes :<|> deletePush :<|> getPush)
  :<|> createEphemeral
  :<|> getMe
  :<|> (getDevices :<|> createDevice :<|> deleteDevice)
  :<|> (getSmsThreads :<|> getSmsMessages)
    = client pushbulletApi

-- | Constructs an authenticator from a pushbullet key.
--
-- This authenticator adds the necessary @Access-Token@ header to the request.
pushbulletAuth :: PushbulletKey -> Auth
pushbulletAuth key = mkAuthenticateReq key f where
  f = addHeader "Access-Token"

-- | A shorter name of the auth type we use.
type Auth = AuthenticateReq (AuthProtect PushbulletAuth)

type instance AuthClientData (AuthProtect PushbulletAuth) = PushbulletKey

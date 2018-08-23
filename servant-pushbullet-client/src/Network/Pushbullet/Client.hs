{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Pushbullet.Client where

import Network.Pushbullet.Api
import Network.Pushbullet.Types

import Servant.Client
import Servant.Client.Core
  ( addHeader
  , AuthClientData
  , AuthenticatedRequest
  , mkAuthenticatedRequest
  )
import Servant.API hiding ( addHeader )

-- | Create a new push.
createPush
  :: Auth
  -> Push 'New
  -> ClientM (Push 'Existing)
-- | List existing pushes.
-- This function should be used in conjunction with 'getPaginatedLimit'.
getPushes
  :: Auth
  -> Maybe PushbulletTime -- ^ Request pushes modified after this time.
  -> Maybe Bool -- ^ Whether to list pushes that have not been deleted.
  -> Maybe Int -- ^ Limit on the number of pushes to return.
  -> Maybe Cursor -- ^ A cursor for use with pagination.
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
pushbulletAuth key = mkAuthenticatedRequest key f where
  f = addHeader "Access-Token"

-- | A shorter name of the auth type we use.
type Auth = AuthenticatedRequest (AuthProtect PushbulletAuth)

type instance AuthClientData (AuthProtect PushbulletAuth) = PushbulletKey

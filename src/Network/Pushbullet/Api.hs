{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Network.Pushbullet.Api where

import Network.Pushbullet.Types

import Data.Proxy
import Data.Text ( Text )
import GHC.TypeLits ( Symbol )
import Servant.API

data PushbulletAuth

newtype PushbulletKey = PushbulletKey Text
  deriving ToHttpApiData

type PushbulletApi
  = "v2" :> PushbulletApiV2

type PushbulletApiV2
  = PushAuth (AuthProtect PushbulletAuth) (
    "pushes" :> (
        ReqBody '[JSON] (Push 'New) :> Post '[JSON] (Push 'Existing)
      :<|>
        QueryParam "modified_after" PushbulletTime
        :> QueryParam "active" Bool
        :> QueryParam "cursor" Cursor
        :> QueryParam "limit" Int
        :> Get '[JSON] (Paginated ExistingPushes)
    )
  :<|>
    "ephemerals"
      :> ReqBody '[JSON] Ephemeral :> Post '[JSON] TrivialObject
  :<|>
    "users"
      :> "me"
        :> Get '[JSON] User
  :<|> "devices" :> (
      QueryParam "active" Bool
      :> QueryParam "cursor" Cursor
      :> Get '[JSON] (Paginated ExistingDevices)
    :<|>
      ReqBody '[JSON] (Device 'New)
      :> Post '[JSON] (Device 'Existing)
    :<|>
      Capture "deviceId" DeviceId
      :> Delete '[JSON] TrivialObject
    )
  :<|>
    "permanents" :> (
        Capture "permanent" (Permanent 'ThreadList)
          :> Get '[JSON] SmsThreads
      :<|>
        Capture "permanent" (Permanent 'MessageList)
          :> Get '[JSON] SmsMessages
    )
  )

pushbulletApi :: Proxy PushbulletApi
pushbulletApi = Proxy

type family PushAuth (auth :: *) (api :: *) :: * where
  PushAuth auth ((s :: Symbol) :> api) = s :> PushAuth auth api
  PushAuth auth (l :<|> r) = PushAuth auth l :<|> PushAuth auth r
  PushAuth auth a = auth :> a

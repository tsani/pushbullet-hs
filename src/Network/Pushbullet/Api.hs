{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Network.Pushbullet.Api where

import Network.Pushbullet.Types

import Data.Aeson ( Object )
import Data.Proxy
import Data.Text ( Text )
import Servant.API
import Servant.API.Experimental.Auth

data PushbulletAuth

newtype PushbulletKey = PushbulletKey Text
  deriving ToHttpApiData

type PushbulletApi
  = "v2" :> AuthProtect PushbulletAuth :> PushbulletApiV2

type PushbulletApiV2
  =
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
      Get '[JSON] (Paginated ExistingDevices)
    :<|>
      ReqBody '[JSON] (Device 'New)
      :> Post '[JSON] (Device 'Existing)
    :<|>
      Capture "deviceId" DeviceId
      :> Delete '[JSON] TrivialObject
  )

pushbulletApi :: Proxy PushbulletApi
pushbulletApi = Proxy

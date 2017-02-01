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
  = "pushes"
    :> ReqBody '[JSON] Push :> Post '[JSON] Object

pushbulletApi :: Proxy PushbulletApi
pushbulletApi = Proxy

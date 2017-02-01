{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Network.Pushbullet.Client where

import Network.Pushbullet.Api
import Network.Pushbullet.Types

import Servant.Client
import Servant.Common.Req ( addHeader )
import Servant.API hiding ( addHeader )

type instance AuthClientData (AuthProtect PushbulletAuth) = PushbulletKey

pushbulletApiClient :: Client PushbulletApi
pushbulletApiClient = client pushbulletApi

pushbulletAuth :: PushbulletKey -> AuthenticateReq (AuthProtect PushbulletAuth)
pushbulletAuth key = mkAuthenticateReq key f where
  f = addHeader "Access-Token"

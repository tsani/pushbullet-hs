module Network.Pushbullet.Internal.Reflection
( Demote'
, Demote
, Reflected
, Reflect(..)
, KProxy(..)
) where

import Data.Proxy

type family Demote' (p :: KProxy k) :: k -> *

type Demote (a :: k) = Demote' ('KProxy :: KProxy k)
type Reflected a = Demote a a

class Reflect a where
  reflect :: proxy a -> Reflected a

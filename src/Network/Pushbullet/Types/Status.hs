module Network.Pushbullet.Types.Status
( Status(..)
, EqT
) where

import Network.Pushbullet.Reflection

data Status
  = New
  | Existing

data Status' :: Status -> * where
  New' :: Status' 'New
  Existing' :: Status' 'Existing

type instance Demote' ('KProxy :: KProxy Status) = Status'

instance Reflect 'New where
  reflect _ = New'

instance Reflect 'Existing where
  reflect _ = Existing'

-- | If the first two types are the same, return the third; else, return unit.
--
-- This type family is used to disable certain fields according to the 'Status'
-- datakind.
type family EqT (s :: k) (s' :: k) (a :: *) :: * where
  EqT s s a = a
  EqT _ _ _ = ()

module Network.Pushbullet.Types.Status
( Status(..)
, EqT
) where

data Status
  = New
  | Existing

-- | If the first two types are the same, return the third; else, return unit.
--
-- This type family is used to disable certain fields according to the 'Status'
-- datakind.
type family EqT (s :: k) (s' :: k) (a :: *) :: * where
  EqT s s a = a
  EqT _ _ _ = ()

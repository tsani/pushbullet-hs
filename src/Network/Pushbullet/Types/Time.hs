module Network.Pushbullet.Types.Time where

import Data.Aeson
import Data.String ( fromString )
import Data.Scientific ( toRealFloat )
import Data.Time.Clock ( NominalDiffTime, UTCTime )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds, posixSecondsToUTCTime )
import Web.HttpApiData ( ToHttpApiData(..) )

newtype PushbulletTime = PushbulletTime UTCTime
  deriving (Eq, Ord, Show)

instance ToHttpApiData PushbulletTime where
  toUrlPiece (PushbulletTime utc) = fromString (show d) where
    d = fromRational r :: Double
    r = toRational $ utcTimeToPOSIXSeconds utc

instance FromJSON PushbulletTime where
  parseJSON (Number n) = pure (PushbulletTime (posixSecondsToUTCTime p)) where
    p = fromRational (toRational d) :: NominalDiffTime
    d = toRealFloat n :: Double
  parseJSON _ = fail "cannot parse pushbullet time from non-number"

instance ToJSON PushbulletTime where
  toJSON (PushbulletTime utcTime)
    = Number $ fromRational (toRational $ utcTimeToPOSIXSeconds utcTime)

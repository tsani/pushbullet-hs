{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Pushbullet.Types.Pagination where

import Data.Aeson
import Data.Text ( Text )
import Lens.Micro.TH
import Web.HttpApiData ( ToHttpApiData(..) )

-- | Cursors are opaque.
newtype Cursor = Cursor Text
  deriving (Eq, FromJSON, Show, ToJSON, ToHttpApiData)

-- | A single page of data, possibly with a cursor attached to it.
-- The cursor may be used in routes that return paginated data to produce the
-- next page of data.
data Paginated a
  = Page
    { _pageData :: !a
    , _pageCursor :: !(Maybe Cursor)
    }
  deriving (Eq, Functor, Show)

makeLenses ''Paginated

instance FromJSON a => FromJSON (Paginated a) where
  parseJSON j@(Object o) = Page <$> parseJSON j <*> o .:? "cursor"
  parseJSON _ = fail "cannot parse paginated data from non-object"

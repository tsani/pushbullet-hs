{-# LANGUAGE OverloadedStrings #-}

module Network.Pushbullet.Types.Pagination where

import Data.Aeson
import Data.Text ( Text )
import Web.HttpApiData ( ToHttpApiData(..) )

-- | A single page of data, possibly with a cursor attached to it.
-- The cursor may be used in routes that return paginated data to produce the
-- next page of data.
data Paginated a = Page !a !(Maybe Cursor)
  deriving (Eq, Functor, Show)

-- | Cursors are opaque.
newtype Cursor = Cursor Text
  deriving (Eq, FromJSON, Show, ToJSON, ToHttpApiData)

instance FromJSON a => FromJSON (Paginated a) where
  parseJSON j@(Object o) = Page <$> parseJSON j <*> o .:? "cursor"
  parseJSON _ = fail "cannot parse paginated data from non-object"

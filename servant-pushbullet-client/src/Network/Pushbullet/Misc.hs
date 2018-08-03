{-|
 - Miscellaneous functions and types for dealing with pushbullet data.
 -}

module Network.Pushbullet.Misc where

import Network.Pushbullet.Types ( Cursor, Paginated(..) )

-- | A count of items to retrieve from the API.
data Count
  = All
  | Limit Int

-- | Retrieves paginated data as a list.
--
-- Suppose we want to retrieve 550 items from a resource with 1100
-- entries and Pushbullet emits pages of 100 items each. This function
-- takes care of repeatedly calling the API and keeping track of the
-- cursors emitted from subsequent calls until those 550 items have
-- been collected.
getPaginatedLimit
  :: Monad m
  => Count -- ^ The count of items you wish to retrieve from the API.
  -> Paginated [a] -- ^ The first page of data to kick off the process.
  -> (Cursor -> m (Paginated [a]))
    -- ^ How to retrieve the next page given a cursor.
  -> m [a]
getPaginatedLimit All (Page d Nothing) _
  = pure d
getPaginatedLimit All (Page d (Just c)) next = do
  p <- next c
  later <- getPaginatedLimit All p next
  pure (d ++ later)
getPaginatedLimit (Limit n) _ _ | n <= 0
  = pure []
getPaginatedLimit (Limit n) (Page d Nothing) _
  = pure (take n d)
getPaginatedLimit (Limit n) (Page d (Just c)) next = do
  let d' = take n d
  let n' = n - length d'
  p <- next c
  later <- getPaginatedLimit (Limit n') p next
  pure (d' ++ later)

-- | A nicer interface to kick-start 'getPaginatedLimit', since many
-- client functions in this library end with @Maybe Cursor -> m [A]@.
-- (Actually, many tend to retrun @m A@ for some @A@ that wraps a list.)
getPaginatedLimit'
  :: Monad m
  => Count
  -> (Maybe Cursor -> m (Paginated [a]))
  -> m [a]
getPaginatedLimit' c next = do
  page <- next Nothing
  getPaginatedLimit c page (next . Just)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Paginator.Paginate
    ( paginate
    , paginate'
    , selectPaginated
    , selectPaginated'
    )
where

import Yesod.Paginator.Prelude

import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist
import Yesod.Core
import Yesod.Paginator.Pages

-- | Paginate a list of items
paginate :: MonadHandler m => PerPage -> [a] -> m (Pages a)
paginate per items = paginate' per items <$> getCurrentPage

-- | A version where the current page is given
--
-- This can be used to avoid the monadic context altogether.
--
-- >>> paginate' 3 ([1..10] :: [Int]) 1
-- Pages {pagesCurrent = Page {pageItems = [1,2,3], pageNumber = 1}, pagesPrevious = [], pagesNext = [2,3,4], pagesLast = 4}
--
-- >>> paginate' 3 ([1..10] :: [Int]) 2
-- Pages {pagesCurrent = Page {pageItems = [4,5,6], pageNumber = 2}, pagesPrevious = [1], pagesNext = [3,4], pagesLast = 4}
--
-- >>> paginate' 3 ([1..10] :: [Int]) 3
-- Pages {pagesCurrent = Page {pageItems = [7,8,9], pageNumber = 3}, pagesPrevious = [1,2], pagesNext = [4], pagesLast = 4}
--
-- >>> paginate' 3 ([1..10] :: [Int]) 4
-- Pages {pagesCurrent = Page {pageItems = [10], pageNumber = 4}, pagesPrevious = [1,2,3], pagesNext = [], pagesLast = 4}
--
-- >>> paginate' 3 ([1..10] :: [Int]) 5
-- Pages {pagesCurrent = Page {pageItems = [], pageNumber = 5}, pagesPrevious = [1,2,3,4], pagesNext = [], pagesLast = 4}
--
paginate' :: PerPage -> [a] -> PageNumber -> Pages a
paginate' per items p =
    toPages p per (genericLength items) $ genericTake per $ genericDrop
        (pageOffset p per)
        items

-- | Paginate out of a persistent database
selectPaginated
    :: ( MonadHandler m
       , PersistEntity record
       , PersistEntityBackend record ~ BaseBackend backend
       , PersistQueryRead backend
       )
    => PerPage
    -> [Filter record]
    -> [SelectOpt record]
    -> ReaderT backend m (Pages (Entity record))
selectPaginated per filters options =
    selectPaginated' per filters options =<< lift getCurrentPage

-- | A version where the current page is given
--
-- This can be used to avoid the @'MonadHandler'@ context.
--
selectPaginated'
    :: ( MonadIO m
       , PersistEntity record
       , PersistEntityBackend record ~ BaseBackend backend
       , PersistQueryRead backend
       )
    => PerPage
    -> [Filter record]
    -> [SelectOpt record]
    -> PageNumber
    -> ReaderT backend m (Pages (Entity record))
selectPaginated' per filters options p =
    toPages p per <$> (fromIntegral <$> count filters) <*> selectList
        filters
        (options
        <> [ OffsetBy $ fromIntegral $ pageOffset p per
           , LimitTo $ fromIntegral per
           ]
        )

getCurrentPage :: MonadHandler m => m PageNumber
getCurrentPage = fromMaybe 1 . go <$> lookupGetParam "p"
  where
    go :: Maybe Text -> Maybe PageNumber
    go mp = readIntegral . unpack =<< mp

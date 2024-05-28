{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Yesod.Paginator.Pages
  ( -- * Type safe @'Natural'@s

    -- |
    --
    -- N.B. @'PageNumber'@ and @'PerPage'@ will currently allow @0@, but it's
    -- unclear if that's correct and may not be the case in the future.
    PageNumber
  , PerPage
  , ItemsCount
  , pageOffset

    -- * Page
  , Page
  , pageItems
  , pageNumber
  , toPage

    -- * Pages
  , Pages
  , pagesCurrent
  , pagesLast
  , toPages

    -- * Safely accessing Pages data
  , takePreviousPages
  , takeNextPages
  , getPreviousPage
  , getNextPage
  ) where

import Yesod.Paginator.Prelude

import Text.Blaze (ToMarkup)
import Web.PathPieces

newtype PageNumber = PageNumber Natural
  deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show, ToMarkup)

newtype PerPage = PerPage Natural
  deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Read, Show, PathPiece)

newtype ItemsCount = ItemsCount Natural
  deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Read, Show, PathPiece)

data Page a = Page
  { pageItems :: [a]
  , pageNumber :: PageNumber
  }
  deriving stock (Eq, Show)

setPageItems :: Page a -> [b] -> Page b
setPageItems page items = page {pageItems = items}

overPageItems :: ([a] -> [b]) -> Page a -> Page b
overPageItems f page = setPageItems page $ f $ pageItems page

instance Functor Page where
  fmap f = overPageItems $ fmap f

instance Foldable Page where
  foldMap f = foldMap f . pageItems

instance Traversable Page where
  traverse f page = setPageItems page <$> traverse f (pageItems page)

-- | @'Page'@ constructor
toPage :: [a] -> PageNumber -> Page a
toPage = Page

data Pages a = Pages
  { pagesCurrent :: Page a
  , pagesPrevious :: [PageNumber]
  , pagesNext :: [PageNumber]
  , pagesLast :: PageNumber
  }
  deriving stock (Eq, Show)

setPagesCurrent :: Pages a -> Page b -> Pages b
setPagesCurrent pages current = pages {pagesCurrent = current}

overPagesCurrent :: (Page a -> Page b) -> Pages a -> Pages b
overPagesCurrent f pages = setPagesCurrent pages $ f $ pagesCurrent pages

instance Functor Pages where
  fmap f = overPagesCurrent $ fmap f

instance Foldable Pages where
  foldMap f = foldMap f . pagesCurrent

instance Traversable Pages where
  traverse f pages =
    setPagesCurrent pages <$> traverse f (pagesCurrent pages)

-- | Take previous pages, going back from current
--
-- >>> takePreviousPages 3 $ Pages (Page [] 5) [1,2,3,4] [6] 6
-- [2,3,4]
takePreviousPages :: Natural -> Pages a -> [PageNumber]
takePreviousPages n = reverse . genericTake n . reverse . pagesPrevious

-- | Take next pages, going forward from current
--
-- >>> takeNextPages 3 $ Pages (Page [] 2) [1] [3,4,5,6] 6
-- [3,4,5]
takeNextPages :: Natural -> Pages a -> [PageNumber]
takeNextPages n = genericTake n . pagesNext

-- | The previous page number, if it exists
--
-- >>> getPreviousPage $ Pages (Page [] 1) [] [2,3,4] 4
-- Nothing
--
-- >>> getPreviousPage $ Pages (Page [] 2) [1] [3,4] 4
-- Just 1
getPreviousPage :: Pages a -> Maybe PageNumber
getPreviousPage pages = do
  let prevPage = pageNumber (pagesCurrent pages) - 1
  firstPage <- headMay $ pagesPrevious pages
  prevPage <$ guard (prevPage >= firstPage)

-- | The next page number, if it exists
--
-- >>> getNextPage $ Pages (Page [] 4) [1,2,3] [] 4
-- Nothing
--
-- >>> getNextPage $ Pages (Page [] 3) [1,2] [4] 4
-- Just 4
getNextPage :: Pages a -> Maybe PageNumber
getNextPage pages = do
  let nextPage = pageNumber (pagesCurrent pages) + 1
  lastPage <- lastMay $ pagesNext pages
  nextPage <$ guard (nextPage <= lastPage)

-- | Construct a @'Pages' a@ from paginated data
--
-- >>> toPages 4 3 10 []
-- Pages {pagesCurrent = Page {pageItems = [], pageNumber = 4}, pagesPrevious = [1,2,3], pagesNext = [], pagesLast = 4}
toPages :: PageNumber -> PerPage -> ItemsCount -> [a] -> Pages a
toPages number per total items =
  Pages
    { pagesCurrent = toPage items number
    , pagesPrevious = [1 .. (number - 1)]
    , pagesNext = [(number + 1) .. lastPage]
    , pagesLast = lastPage
    }
 where
  lastPage = getLastPage total per

-- | Calculate the last page of some paginated data
--
-- >>> getLastPage 10 3
-- 4
--
-- >>> getLastPage 10 5
-- 2
getLastPage :: ItemsCount -> PerPage -> PageNumber
getLastPage total = fromIntegral . carry . (total `divMod`) . fromIntegral
 where
  carry :: (Eq a, Num a) => (a, a) -> a
  carry (q, 0) = q
  carry (q, _) = q + 1

-- | Calculate a page's zero-based offset in the overall items
--
-- >>> pageOffset 4 3
-- 9
pageOffset :: PageNumber -> PerPage -> ItemsCount
pageOffset p per = fromIntegral $ (fromIntegral p - 1) * per

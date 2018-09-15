-- |
--
-- There are two pagination functions. One for arbitrary items where you provide
-- the list of things to be paginated:
--
-- @
-- getSomeRoute = do
--     -- 10 items per page
--     pages <- 'paginate' 10 =<< getAllThings
--
--     defaultLayout $ do
--         [whamlet|
--             $forall thing <- 'pageItems' $ 'pagesCurrent' pages
--                 ^{showThing thing}
--
--             $# display at most 5 page elements, with current page middle-ish
--             ^{'simple' 5 pages}
--             |]
-- @
--
-- And another for paginating directly out of the database, you provide the same
-- arguments as you would for @'selectList'@:
--
-- @
-- getSomeRoute something = do
--     pages <- runDB $ 'selectPaginated' 10 [SomeThing ==. something] []
--
--     defaultLayout $ do
--         [whamlet|
--             $forall thing <- 'pageItems' $ 'pagesCurrent' pages
--                 ^{showThing $ entityVal thing}
--
--             ^{'simple' 5 pages}
--             |]
-- @
--
-- For backends other than persitent @'selectCustom'@ can be used
--
-- @
-- getSomeRoute something = do
--     (totCnt, pages) <- runDB $ 'selectCustom' 10
--                  someCntQuery
--                  (\pn -> someOffsetQuery $ 10 * (fromIntegral pn - 1))
--
--     defaultLayout $ do
--         [whamlet|
--             $forall thing <- 'pageItems' $ 'pagesCurrent' pages
--                 ^{showThing $ entityVal thing}
--
--             ^{'simple' 5 pages}
--             |]
-- @

module Yesod.Paginator
    (
    -- * Type-safe numerics
      PageNumber
    , PerPage
    , ItemsCount

    -- * Paginated data
    , Pages
    , pagesCurrent
    , pageOffset

    -- * The current page
    , Page
    , pageItems

    -- * Paginators
    , paginate
    , selectPaginated
    , paginateCustom

    -- * Widgets
    , simple
    , ellipsed
    )
where

import Yesod.Paginator.Pages
import Yesod.Paginator.Paginate
import Yesod.Paginator.Widgets

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-------------------------------------------------------------------------------
-- |
--
-- Inspiration from a concept by ajdunlap:
--      <http://hackage.haskell.org/package/yesod-paginate>
--
-- But uses an entirely different approach.
--
-- There are two pagination functions. One for arbitrary items where you
-- provide the list of things to be paginated:
--
-- > getSomeRoute = do
-- >     things' <- getAllThings
-- >
-- >     (things, widget) <- paginate 10 things'
-- >
-- >     defaultLayout $ do
-- >         [whamlet|
-- >             $forall thing <- things
-- >                 ^{showThing thing}
-- >
-- >             <div .pagination>
-- >                  ^{widget}
-- >             |]
--
-- And another for paginating directly out of the database, you provide
-- the same filters as you would to @selectList@.
--
-- > getSomeRoute something = do
-- >     -- note: things is [Entity val] just like selectList returns
-- >     (things, widget) <- runDB $ selectPaginated 10 [SomeThing ==. something] []
-- >
-- >     defaultLayout $ do
-- >         [whamlet|
-- >             $forall thing <- things
-- >                 ^{showThing $ entityVal thing}
-- >
-- >             <div .pagination>
-- >                  ^{widget}
-- >             |]
--
-- Both functions return a tuple: the first element being the list of
-- items (or Entities) to display on this page and the second being a
-- widget showing the pagination navagation links.
--
--
-- The third example omits the widget and provides some flexibility in terms
-- of naming the pagination parameter (can be useful if returning JSON)
--
-- > getSomeRoute :: Handler Value
-- > getSomeRoute something = do
-- >     -- note: you can use paginators function getCurrentPage or your own implementation
-- >     page   <- getCurrentPage
-- >
-- >     -- note: things is [Entity val] just like selectList returns
-- >     things <- runDB $ selectPaginatedEntities page 10 [SomeThing ==. something] []
-- >
-- >     returnJson things
--
--
-------------------------------------------------------------------------------
module Yesod.Paginator
    ( paginate
    , paginateWith
    , selectPaginated
    , selectPaginatedWith
    , selectPaginatedEntities
    , module Yesod.Paginator.Widget
    ) where

import Yesod
import Yesod.Paginator.Widget



paginate :: Yesod m => Int -> [a] -> HandlerT m IO ([a], WidgetT m IO ())
paginate = paginateWith defaultWidget

paginateWith :: Yesod m
             => PageWidget m
             -> Int
             -> [a]
             -> HandlerT m IO ([a], WidgetT m IO ())
paginateWith widget itemsPerPage items = do
    page <- getCurrentPage

    let total = length items
    let  xs   = take itemsPerPage $ drop ((page - 1) * itemsPerPage) items

    pure (xs, widget page itemsPerPage total)



selectPaginated :: ( PersistEntity val
#if MIN_VERSION_persistent(2, 5, 0)
                   , PersistEntityBackend val ~ BaseBackend (YesodPersistBackend m)
#else
                   , PersistEntityBackend val ~ YesodPersistBackend m
#endif
                   , PersistQuery (YesodPersistBackend m)
                   , Yesod m
                   )
                => Int
                -> [Filter val]
                -> [SelectOpt val]
                -> YesodDB m ([Entity val], WidgetT m IO ())
selectPaginated = selectPaginatedWith defaultWidget



selectPaginatedWith :: ( PersistEntity val
#if MIN_VERSION_persistent(2, 5, 0)
                       , PersistEntityBackend val ~ BaseBackend (YesodPersistBackend m)
#else
                       , PersistEntityBackend val ~ YesodPersistBackend m
#endif
                       , PersistQuery (YesodPersistBackend m)
                       , Yesod m
                       )
                    => PageWidget m
                    -> Int
                    -> [Filter val]
                    -> [SelectOpt val]
                    -> YesodDB m ([Entity val], WidgetT m IO ())
selectPaginatedWith widget itemsPerPage filters selectOpts = do
    page   <- lift getCurrentPage
    total  <- count filters
    let paginationOptions = [OffsetBy ((page-1)*itemsPerPage), LimitTo itemsPerPage]
    xs     <- selectList filters (selectOpts ++ paginationOptions)

    pure (xs, widget page itemsPerPage total)



selectPaginatedEntities :: ( PersistEntity val
#if MIN_VERSION_persistent(2, 5, 0)
                   , PersistEntityBackend val ~ BaseBackend (YesodPersistBackend m)
#else
                   , PersistEntityBackend val ~ YesodPersistBackend m
#endif
                   , PersistQuery (YesodPersistBackend m)
                   , Yesod m
                   )
                => Int
                -> Int
                -> [Filter val]
                -> [SelectOpt val]
                -> YesodDB m [Entity val]
selectPaginatedEntities page itemsPerPage filters selectOpts = do
    let paginationOptions = [OffsetBy ((page-1)*itemsPerPage), LimitTo itemsPerPage]
    xs     <- selectList filters (selectOpts ++ paginationOptions)

    pure xs

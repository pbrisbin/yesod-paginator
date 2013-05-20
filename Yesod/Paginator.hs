{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
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
-------------------------------------------------------------------------------
module Yesod.Paginator
    ( paginate
    , paginateWith
    , selectPaginated
    , selectPaginatedWith
    , module Yesod.Paginator.Widget
    ) where

import Control.Monad.Trans.Resource
import Yesod
import Yesod.Paginator.Widget

paginate :: (MonadThrow m, MonadBaseControl IO m, MonadUnsafeIO m, MonadHandler m) => Int -> [a] -> HandlerT s m ([a], WidgetT s m ())
paginate = paginateWith defaultWidget

paginateWith :: (MonadThrow m, MonadIO m, MonadUnsafeIO m, MonadBaseControl IO m) => PageWidget s m -> Int -> [a] -> HandlerT s m ([a], WidgetT s m ())
paginateWith widget per items = do
    p <- getCurrentPage

    let tot = length items
    let  xs = take per $ drop ((p - 1) * per) items

    return (xs, widget p per tot)

selectPaginated :: ( PersistEntity val
                   , PersistQuery (HandlerT s m)
                   , PersistEntityBackend val ~ PersistMonadBackend (HandlerT s m)
                   , MonadThrow m
                   , MonadBaseControl IO m
                   , MonadHandler m
                   )
                => Int
                -> [Filter val]
                -> [SelectOpt val]
                -> HandlerT s m ([Entity val], WidgetT s m ())
selectPaginated = selectPaginatedWith defaultWidget

selectPaginatedWith :: ( PersistEntity val
                       , PersistQuery (HandlerT s m)
                       , PersistEntityBackend val ~ PersistMonadBackend (HandlerT s m)
                       , MonadThrow m
                       , MonadUnsafeIO m
                       , MonadBaseControl IO m
                       , MonadHandler m
                       )
                    => PageWidget s m
                    -> Int
                    -> [Filter val]
                    -> [SelectOpt val]
                    -> HandlerT s m ([Entity val], WidgetT s m ())
selectPaginatedWith widget per filters selectOpts = do
    p   <- getCurrentPage
    tot <- count filters
    xs  <- selectList filters (selectOpts ++ [OffsetBy ((p-1)*per), LimitTo per])

    return (xs, widget p per tot)

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
    , selectPaginated
    , paginationWidget
    ) where

import Yesod
import Yesod.Paginator.Widget
import Control.Monad.Trans.Class (MonadTrans)

paginate :: Int -> [a] -> GHandler s m ([a], GWidget s m ())
paginate per items = do
    p <- getCurrentPage

    let tot = length items
    let  xs = take per $ drop ((p - 1) * per) items

    return (xs, paginationWidget p per tot)

selectPaginated :: ( MonadTrans (PersistEntityBackend v)
                   , PersistEntity v
                   , PersistQuery (PersistEntityBackend v) (GHandler s m))
                => Int -> [Filter v] -> [SelectOpt v]
                -> PersistEntityBackend v (GHandler s m) ([Entity v], GWidget s1 m1 ())
selectPaginated per filters selectOpts = do
    p   <- lift getCurrentPage
    tot <- count filters
    xs  <- selectList filters (selectOpts ++ [OffsetBy ((p-1)*per), LimitTo per])

    return (xs, paginationWidget p per tot)

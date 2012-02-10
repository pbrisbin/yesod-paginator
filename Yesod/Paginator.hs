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
-- provide the list of things to be paginated, and one for paginating
-- directly out of the database, you provide the same filters as you
-- would to @selectList@.
--
-- Both functions return a tuple: the first element being the list of
-- items to display on this page and the second being a widget showing
-- the pagination navagation links.
--
-------------------------------------------------------------------------------
module Yesod.Paginator
    ( paginate
    , selectPaginated
    , paginationWidget
    ) where

import Yesod
import Database.Persist.Store (Entity)
import Data.Text (Text)
import qualified Data.Text as T

-- | Paginate an existing list of items.
--
--   > getSomeRoute = do
--   >     things' <- getAllThings
--   >
--   >     (things, widget) <- paginate 10 things'
--   >
--   >     defaultLayout $ do
--   >         [whamlet|
--   >             $forall thing <- things
--   >                 ^{showThing thing}
--   >
--   >             <div .pagination>
--   >                  ^{widget}
--   >             |]
--
paginate :: Int -- ^ items per page
         -> [a] -- ^ complete list of items
         -> GHandler s m ([a], GWidget s m ())
paginate per items = do
    let tot = length items

    p <- getCurrentPage tot

    let xs = take per $ drop ((p - 1) * per) items

    return (xs, paginationWidget p per tot)

-- | Paginate directly out of the database.
--
--   > getSomeRoute something = do
--   >     -- note: things is [Entity val] just like selectList returns
--   >     (things, widget) <- selectPaginated 10 [SomeThing ==. something] []
--   >
--   >     defaultLayout $ do
--   >         [whamlet|
--   >             $forall thing <- things
--   >                 ^{showThing $ entityVal thing}
--   >
--   >             <div .pagination>
--   >                  ^{widget}
--   >             |]
--
selectPaginated :: ( YesodPersistBackend m ~ PersistEntityBackend v
                   , YesodPersist m
                   , PersistEntity v
                   , PersistQuery (PersistEntityBackend v) (GHandler s m)
                   )
                => Int -> [Filter v] -> [SelectOpt v]
                -> GHandler s m ([Entity v], GWidget s m ())
selectPaginated per filters selectOpts = do
    tot <- runDB $ count filters
    p   <- getCurrentPage tot
    xs  <- runDB $ selectList filters (selectOpts ++ [OffsetBy ((p-1)*per), LimitTo per])

    return (xs, paginationWidget p per tot)

-- | A widget showing pagination links. Follows bootstrap principles.
--   Utilizes a \"p\" GET param but leaves all other GET params intact.
paginationWidget :: Int -- ^ current page
                 -> Int -- ^ items per page
                 -> Int -- ^ total number of items
                 -> GWidget s m ()
paginationWidget page per tot = do
    let pages = (\(n, r) -> n + (min r 1)) $ tot `divMod` per

    if pages <= 1
        then return ()
        else do
            let prev = [1       ..(page-1)]
            let next = [(page+1)..pages   ]

            let lim = 9 -- don't show more than nine links on either side
            let prev' = if length prev > lim then drop ((length prev) - lim) prev else prev
            let next' = if length next > lim then take lim next else next

            curParams <- lift $ fmap reqGetParams getRequest

            [whamlet|
                <ul>
                    <li .prev :null prev:.disabled>
                        ^{linkTo curParams (page - 1) "← Previous"}

                    $if (/=) prev prev'
                        <li>^{linkTo curParams 1 "1"}
                        <li>...

                    $forall p <- prev'
                        <li>^{linkTo curParams p (show p)}

                    <li .active>
                        <a href="#">#{show page}

                    $forall n <- next'
                        <li>^{linkTo curParams n (show n)}

                    $if (/=) next next'
                        <li>...
                        <li>^{linkTo curParams tot (show tot)}

                    <li .next :null next:.disabled>
                        ^{linkTo curParams (page + 1) "Next →"}
                |]

getCurrentPage :: Int -> GHandler s m Int
getCurrentPage tot = do
    mp <- lookupGetParam "p"
    return $
        case mp of
            Nothing -> 1
            Just "" -> 1
            Just p  ->
                case readIntegral $ T.unpack p of
                    Just i -> if i > tot then tot else i
                    _      -> 1

updateGetParam :: [(Text,Text)] -> (Text,Text) -> Text
updateGetParam getParams (p, n) = (T.cons '?') . T.intercalate "&"
                                . map (\(k,v) -> k `T.append` "=" `T.append` v)
                                . (++ [(p, n)]) . filter ((/= p) . fst) $ getParams

linkTo :: [(Text,Text)] -> Int -> String -> GWidget s m ()
linkTo params pg txt = do
    let param = ("p", T.pack $ show pg)

    [whamlet|
        <a href="#{updateGetParam params param}">#{txt}
        |]

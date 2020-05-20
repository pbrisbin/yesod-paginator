{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Yesod.Paginator.Widgets
    ( PaginationWidget
    , simple
    , simpleWith
    , ellipsed
    , ellipsedWith
    )
where

import Yesod.Paginator.Prelude

import qualified Data.Text as T
import Network.URI.Encode (encodeText)
import Yesod.Core
import Yesod.Paginator.Pages
import Yesod.Paginator.Paginate

type PaginationWidget site a = Pages a -> WidgetFor site ()

-- | Simple widget, limited to show the given number of total page elements
--
-- Pseudo-HTML for @'simple' 5@, on page 1:
--
-- @
--   \<ul .pagination>
--     \<li .prev .disabled>\<a>«
--     \<li .active .disabled>\<a>1
--     \<li .next>\<a href=\"?p=2\">2
--     \<li .next>\<a href=\"?p=3\">3
--     \<li .next>\<a href=\"?p=4\">4
--     \<li .next>\<a href=\"?p=5\">5
--     \<li .next>\<a href=\"?p=2\">»
-- @
--
-- And page 7:
--
-- @
--   \<ul .pagination>
--     \<li .prev>\<a href=\"?p=6\">«
--     \<li .prev>\<a href=\"?p=5\">5
--     \<li .prev>\<a href=\"?p=6\">6
--     \<li .active .disabled>\<a>7
--     \<li .next>\<a href=\"?p=8\">8
--     \<li .next>\<a href=\"?p=9\">9
--     \<li .next>\<a href=\"?p=8\">»
-- @
--
simple :: Natural -> PaginationWidget m a
simple = simpleWith defaultPaginationConfig

simpleWith :: PaginationConfig -> Natural -> PaginationWidget m a
simpleWith config elements pages = do
    updateGetParams <- getUpdateGetParams (paginationConfigPageParamName config)

    let (prevPages, nextPages) = getBalancedPages elements pages
        mPrevPage = getPreviousPage pages
        mNextPage = getNextPage pages

    [whamlet|$newline never
        <ul .pagination>
            $maybe prevPage <- mPrevPage
                <li .prev>
                    <a href=#{renderGetParams $ updateGetParams prevPage}>«
            $nothing
                <li .prev .disabled>
                    <a>«
            $forall number <- prevPages
                <li .prev >
                    <a href=#{renderGetParams $ updateGetParams number}>#{number}
            $with number <- pageNumber $ pagesCurrent pages
                <li .active .disabled>
                    <a>#{number}
            $forall number <- nextPages
                <li .next>
                    <a href=#{renderGetParams $ updateGetParams number}>#{number}
            $maybe nextPage <- mNextPage
                <li .next>
                    <a href=#{renderGetParams $ updateGetParams nextPage}>»
            $nothing
                <li .next .disabled>
                    <a>»
        |]

-- | Show pages before and after, ellipsis, and first/last
ellipsed :: Natural -> PaginationWidget m a
ellipsed = ellipsedWith defaultPaginationConfig

ellipsedWith :: PaginationConfig -> Natural -> PaginationWidget m a
ellipsedWith config elements pages = do
    updateGetParams <- getUpdateGetParams (paginationConfigPageParamName config)

    let (prevPages, nextPages) = getBalancedPages elements pages

        mPrevPage = getPreviousPage pages
        mNextPage = getNextPage pages

        (mFirstPage, firstEllipses)
            | pageNumber (pagesCurrent pages) == 1 = (Nothing, False)
            | headMay prevPages == Just 1 = (Nothing, False)
            | headMay prevPages == Just 2 = (Just 1, False)
            | otherwise = (Just 1, True)

        (mLastPage, lastEllipses)
            | pageNumber (pagesCurrent pages) == pagesLast pages = (Nothing, False)
            | lastMay nextPages == Just (pagesLast pages) = (Nothing, False)
            | lastMay nextPages == Just (pagesLast pages - 1) = (Just $ pagesLast pages, False)
            | otherwise = (Just $ pagesLast pages, True)

    [whamlet|$newline never
        <ul .pagination>
            $maybe prevPage <- mPrevPage
                <li .prev>
                    <a href=#{renderGetParams $ updateGetParams prevPage}>«
            $nothing
                <li .prev .disabled>
                    <a>«
            $maybe firstPage <- mFirstPage
                <li .prev>
                    <a href=#{renderGetParams $ updateGetParams firstPage}>#{firstPage}
                $if firstEllipses
                    <li .prev .disabled>
                        <a>…
            $forall number <- prevPages
                <li .prev >
                    <a href=#{renderGetParams $ updateGetParams number}>#{number}
            $with number <- pageNumber $ pagesCurrent pages
                <li .active .disabled>
                    <a>#{number}
            $forall number <- nextPages
                <li .next>
                    <a href=#{renderGetParams $ updateGetParams number}>#{number}
            $maybe lastPage <- mLastPage
                $if lastEllipses
                    <li .next .disabled>
                        <a>…
                <li .next>
                    <a href=#{renderGetParams $ updateGetParams lastPage}>#{lastPage}
            $maybe nextPage <- mNextPage
                <li .next>
                    <a href=#{renderGetParams $ updateGetParams nextPage}>»
            $nothing
                <li .next .disabled>
                    <a>»
        |]

-- | Calculate previous and next pages to produce an overall number of elements
--
-- >>> let page n = toPages n 2 20 [] :: Pages Int
-- >>> getBalancedPages 6 $ page 1
-- ([],[2,3,4,5,6])
--
-- >>> getBalancedPages 6 $ page 6
-- ([3,4,5],[7,8])
--
-- >>> getBalancedPages 6 $ page 10
-- ([5,6,7,8,9],[])
--
getBalancedPages :: Natural -> Pages a -> ([PageNumber], [PageNumber])
getBalancedPages elements pages =
    if genericLength nextPages >= (elements `div` 2)
        then (prevPagesNaive, nextPages)
        else (prevPagesCalcd, nextPages)
  where
    nextPages = takeNextPages (elements - genericLength prevPagesNaive - 1) pages
    prevPagesNaive = takePreviousPages (elements `div` 2) pages
    prevPagesCalcd = takePreviousPages (elements - genericLength nextPages - 1) pages

getUpdateGetParams :: PageParamName -> WidgetFor site (PageNumber -> [(Text, Text)])
getUpdateGetParams pageParamName = do
    params <- handlerToWidget $ reqGetParams <$> getRequest
    pure $ \number -> nubOn fst $ [(unPageParamName pageParamName, tshow number)] <> params

renderGetParams :: [(Text, Text)] -> Text
renderGetParams [] = ""
renderGetParams ps = "?" <> T.intercalate "&" (map renderGetParam ps)
    where renderGetParam (k, v) = encodeText k <> "=" <> encodeText v

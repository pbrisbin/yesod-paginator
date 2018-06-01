{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Yesod.Paginator.Widgets
    ( PaginationWidget
    , simple
    )
where

import Yesod.Paginator.Prelude

import qualified Data.Text as T
import Network.URI.Encode (encodeText)
import Yesod.Core
import Yesod.Paginator.Pages

type PaginationWidget m a = Pages a -> WidgetT m IO ()

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
simple elements pages = do
    params <- handlerToWidget $ reqGetParams <$> getRequest

    let updateGetParams :: PageNumber -> [(Text, Text)]
        updateGetParams number = nubOn fst $ [("p", tshow number)] <> params

        (prevPages, nextPages) = getBalancedPages elements pages

        mPrevPage = getPreviousPage pages
        mNextPage = getNextPage pages

    [whamlet|$newline never
        <ul .pagination>
            $# Move previous, disabled if on first page
            $maybe prevPage <- mPrevPage
                <li .prev>
                    <a href=#{renderGetParams $ updateGetParams prevPage}>«
            $nothing
                <li .prev .disabled>
                    <a>«

            $# Previous pages
            $forall number <- prevPages
                <li .prev >
                    <a href=#{renderGetParams $ updateGetParams number}>#{number}

            $# Current page
            $with number <- pageNumber $ pagesCurrent pages
                <li .active .disabled>
                    <a>#{number}

            $# Next pages
            $forall number <- nextPages
                <li .next>
                    <a href=#{renderGetParams $ updateGetParams number}>#{number}

            $# Move next, disabled if on last page
            $maybe nextPage <- mNextPage
                <li .next>
                    <a href=#{renderGetParams $ updateGetParams nextPage}>»
            $nothing
                <li .next .disabled>
                    <a>»
        |]

getBalancedPages :: Natural -> Pages a -> ([PageNumber], [PageNumber])
getBalancedPages elements pages =
    if genericLength nextPages >= (elements `div` 2)
        then (prevPagesNaive, nextPages)
        else (prevPagesCalcd, nextPages)
  where
    nextPages = takeNextPages (elements - genericLength prevPagesNaive - 1) pages
    prevPagesNaive = takePreviousPages (elements `div` 2) pages
    prevPagesCalcd = takePreviousPages (elements - genericLength nextPages - 1) pages

renderGetParams :: [(Text, Text)] -> Text
renderGetParams [] = ""
renderGetParams ps = "?" <> T.intercalate "&" (map renderGetParam ps)
    where renderGetParam (k, v) = encodeText k <> "=" <> encodeText v

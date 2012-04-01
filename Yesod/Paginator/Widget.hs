{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Paginator.Widget
 ( getCurrentPage  
 , paginationWidget
 ) where

import Yesod

import Control.Monad (when)
import Data.Maybe    (fromMaybe)
import Data.Text     (Text)
import qualified Data.Text as T

-- | A widget showing pagination links. Follows bootstrap principles.
--   Utilizes a \"p\" GET param but leaves all other GET params intact.
paginationWidget :: Int -- ^ current page
                 -> Int -- ^ items per page
                 -> Int -- ^ total number of items
                 -> GWidget s m ()
paginationWidget page per tot = do
    -- total / per + 1 for any remainder
    let pages = (\(n, r) -> n + (min r 1)) $ tot `divMod` per

    when (pages > 1) $ do
        let prev = [1       ..(page-1)]
        let next = [(page+1)..pages   ]

        let lim = 9 -- don't show more than nine links on either side
        let prev' = if length prev > lim then drop ((length prev) - lim) prev else prev
        let next' = if length next > lim then take lim next else next

        curParams <- lift $ fmap reqGetParams getRequest

        [whamlet|
            <ul>
                ^{linkToDisabled (null prev) curParams (page - 1) "← Previous"}

                $if (/=) prev prev'
                    <li>^{linkTo curParams 1 "1"}
                    <li>
                        <a>...

                $forall p <- prev'
                    <li>^{linkTo curParams p (show p)}

                <li .active>
                    <a>#{show page}

                $forall n <- next'
                    <li>^{linkTo curParams n (show n)}

                $if (/=) next next'
                    <li>
                        <a>...
                    <li>^{linkTo curParams pages (show pages)}

                ^{linkToDisabled (null next) curParams (page + 1) "Next →"}
            |]

-- | looks up the \"p\" GET param and converts it to an Int. returns a
--   default of 1 when conversion fails.
getCurrentPage :: GHandler s m Int
getCurrentPage = fmap (fromMaybe 1 . go) $ lookupGetParam "p"

    where
        go :: Maybe Text -> Maybe Int
        go mp = readIntegral . T.unpack =<< mp

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

-- | Similiar, but used for Previous/Next so that there's no href when
--   disabled
linkToDisabled :: Bool -- ^ disabled?
               -> [(Text,Text)] -> Int -> String -> GWidget s m ()
linkToDisabled True _ _ txt = [whamlet|
    <li .prev .disabled>
        <a>#{txt}
    |]

linkToDisabled _ params pg txt = [whamlet|
    <li .prev>
        ^{linkTo params pg txt}
    |]

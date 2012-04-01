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

-- | Individual links to pages need to follow strict (but sane) markup
--   to be styled correctly by bootstrap. This type allows construction
--   of such links in both enabled and disabled states.
data PageLink = Enabled Int String String -- ^ page, content, class
              | Disabled    String String -- ^ content, class

-- | Correctly show one of the constructed links
showLink :: [(Text, Text)] -> PageLink -> GWidget s m ()
showLink params (Enabled pg cnt cls) = do
    let param = ("p", T.pack . show $ pg)

    [whamlet|
        <li .#{cls}>
            <a href="#{updateGetParam params param}">#{cnt}
        |]

    where
        updateGetParam :: [(Text,Text)] -> (Text,Text) -> Text
        updateGetParam getParams (p, n) = (T.cons '?') . T.intercalate "&"
                                        . map (\(k,v) -> k `T.append` "=" `T.append` v)
                                        . (++ [(p, n)]) . filter ((/= p) . fst) $ getParams

showLink _ (Disabled cnt cls) =
    [whamlet|
        <li .#{cls} .disabled>
            <a>#{cnt}
        |]

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
        curParams <- lift $ fmap reqGetParams getRequest

        [whamlet|
            <ul>
                $forall link <- buildLinks page pages
                    ^{showLink curParams link}
            |]

    where
        -- | Build up each component of the overall list of links. We'll
        --   use empty lists to denote ommissions along the way then
        --   concatenate.
        buildLinks :: Int -> Int -> [PageLink]
        buildLinks pg pgs =
            let prev = [1      .. pg - 1]
                next = [pg + 1 .. pgs   ]

                -- these always appear
                prevLink = [(if null prev then Disabled else Enabled (pg - 1)) "«" "prev"]
                nextLink = [(if null next then Disabled else Enabled (pg + 1)) "»" "next"]

                -- show first/last unless we're on it
                firstLink = [ Enabled 1   "1"        "prev" | pg > 1   ]
                lastLink  = [ Enabled pgs (show pgs) "next" | pg < pgs ]

                lim = 9

                -- we'll show ellipsis if there are enough links that some will
                -- be ommitted from the list
                prevEllipsis = [ Disabled "..." "prev" | length prev > lim + 1 ]
                nextEllipsis = [ Disabled "..." "next" | length next > lim + 1 ]

                -- the middle lists, strip the first/last pages and
                -- correctly take up to limit away from current
                prevLinks = reverse . take lim . reverse . drop 1 $ map (\p -> Enabled p (show p) "prev") prev
                nextLinks = take lim . reverse . drop 1 . reverse $ map (\p -> Enabled p (show p) "next") next

                -- finally, this page itself
                curLink = [Disabled (show pg) "active"]

            in concat [ prevLink
                      , firstLink
                      , prevEllipsis
                      , prevLinks
                      , curLink
                      , nextLinks
                      , nextEllipsis
                      , lastLink
                      , nextLink
                      ]

-- | looks up the \"p\" GET param and converts it to an Int. returns a
--   default of 1 when conversion fails.
getCurrentPage :: GHandler s m Int
getCurrentPage = fmap (fromMaybe 1 . go) $ lookupGetParam "p"

    where
        go :: Maybe Text -> Maybe Int
        go mp = readIntegral . T.unpack =<< mp

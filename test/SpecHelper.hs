{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module SpecHelper
    ( module SpecHelper
    , module X
    ) where

import qualified Data.List as List
import Test.Hspec as X
import Yesod.Core
import Yesod.Paginator as X
import Yesod.Paginator.Prelude as X
import Yesod.Paginator.Widgets as X
import Yesod.Test as X

data App = App

mkYesod "App" [parseRoutes|
    /simple/#ItemsCount/#PerPage/#Natural SimpleR GET
    /ellipsed/#ItemsCount/#PerPage/#Natural EllipsedR GET
    /simple-param-name/#ItemsCount/#PerPage/#Natural/#PageParamName SimpleParamNameR GET
    /ellipsed-param-name/#ItemsCount/#PerPage/#Natural/#PageParamName EllipsedParamNameR GET
|]

instance Yesod App

getSimpleR :: ItemsCount -> PerPage -> Natural -> Handler Html
getSimpleR total per elements = do
    pages <- paginate per $ genericReplicate total ()
    defaultLayout [whamlet|^{simple elements pages}|]

getEllipsedR :: ItemsCount -> PerPage -> Natural -> Handler Html
getEllipsedR total per elements = do
    pages <- paginate per $ genericReplicate total ()
    defaultLayout [whamlet|^{ellipsed elements pages}|]

getSimpleParamNameR
    :: ItemsCount -> PerPage -> Natural -> PageParamName -> Handler Html
getSimpleParamNameR total per elements pageParamName = do
    let
        config = PaginationConfig
            { paginationConfigPerPage = per
            , paginationConfigPageParamName = pageParamName
            }
    pages <- paginateWith config $ genericReplicate total ()
    defaultLayout [whamlet|^{simpleWith config elements pages}|]

getEllipsedParamNameR
    :: ItemsCount -> PerPage -> Natural -> PageParamName -> Handler Html
getEllipsedParamNameR total per elements pageParamName = do
    let
        config = PaginationConfig
            { paginationConfigPerPage = per
            , paginationConfigPageParamName = pageParamName
            }
    pages <- paginateWith config $ genericReplicate total ()
    defaultLayout [whamlet|^{ellipsedWith config elements pages}|]

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ pure (App, id)

shouldIncludeAll
    :: (Foldable t, Eq a, Show a, Show (t a)) => t a -> [a] -> Expectation
shouldIncludeAll actual subset = expectTrue msg (all isIncluded subset)
  where
    isIncluded = (`elem` actual)
    msg =
        show actual
            <> " did not include all of "
            <> show subset
            <> " - missing: "
            <> List.intercalate
                   ", "
                   (fmap show (filter (not . isIncluded) subset))

-- Cloned from 'Test.Hspec.Expectations'
expectTrue :: HasCallStack => String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)

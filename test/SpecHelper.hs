{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module SpecHelper
    ( module SpecHelper
    , module X
    )
where

import Test.Hspec as X
import Yesod.Core
import Yesod.Paginator as X
import Yesod.Paginator.Prelude as X
import Yesod.Test as X

data App = App

mkYesod "App" [parseRoutes|
    /#ItemsCount/#PerPage/#Natural SimpleR GET
|]

instance Yesod App

getSimpleR :: ItemsCount -> PerPage -> Natural -> Handler Html
getSimpleR total per elements = do
    pages <- paginate per $ genericReplicate total ()
    defaultLayout [whamlet|^{simple elements pages}|]

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ pure (App, id)

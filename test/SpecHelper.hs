{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module SpecHelper
    ( module SpecHelper
    , module X
    ) where

import Yesod.Core
import Yesod.Paginator (paginate)

import Test.Hspec as X
import Yesod.Test as X

data App = App

mkYesod "App" [parseRoutes| /#Int RootR GET |]

instance Yesod App

getRootR :: Int -> Handler Html
getRootR count = do
    let things' = [1..count]

    (things, widget) <- paginate 3 things'

    defaultLayout [whamlet|
        <ul>
            $forall thing <- things
                <li>#{show thing}

        ^{widget}
    |]

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ pure (App, id)

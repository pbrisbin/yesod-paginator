{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module SpecHelper
    ( module SpecHelper
    , module X
    ) where

import Data.Default (def)
import Network.Wai.Middleware.RequestLogger (mkRequestLogger)
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
withApp = before $ do
    logger <- mkRequestLogger def

    return (App, logger)

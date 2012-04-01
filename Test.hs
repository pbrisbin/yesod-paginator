{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Minimal where

import Yesod
import Yesod.Paginator
import Network.Wai.Handler.Warp (run)

data Minimal = Minimal

mkYesod "Minimal" [parseRoutes|
    / RootR GET
|]

instance Yesod Minimal where 
    approot = ApprootRelative
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        hamletToRepHtml [hamlet|
            \<!DOCTYPE html>
            <html lang="en">
                <head>
                    <meta charset="utf-8">
                    <title>#{pageTitle pc}
                    <link rel="stylesheet" href="http://pbrisbin.com/static/css/bootstrap.min.css">
                    ^{pageHead pc}
                <body>
                    ^{pageBody pc}
            |]

-- Note this just tests the markup that the widget produces. The actual
-- pagination math is less error-prone -- though I should add tests for
-- it eventually...
getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    setTitle "My title"
    
    [whamlet|
        <div .pagination>
            <!--               current page, items per page, total number of items -->
            ^{paginationWidget 12            10              111}
        |]

main :: IO ()
main = run 3000 =<< toWaiApp Minimal

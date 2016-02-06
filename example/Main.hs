{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Yesod
import Yesod.Paginator
import Network.Wai.Handler.Warp (run)

data App = App

mkYesod "App" [parseRoutes|
    / RootR GET
|]

instance Yesod App where
    approot = ApprootRelative
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        withUrlRenderer [hamlet|$newline never
            $doctype 5
            <html lang="en">
                <head>
                    <meta charset="utf-8">
                    <title>#{pageTitle pc}
                    <!-- Get Boostrap from CDN -->
                    <link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" rel="stylesheet" integrity="sha256-7s5uDGW3AHqw6xtJmNNtr+OBRJUlgkNJEo78P4b0yRw= sha512-nNo+yCHEyn0smMxSswnf/OnX6/KwJuZTlNZBjauKhTK0c+zT+q5JOCx0UFhXQ6rJR9jg6Es8gPuD2uZcYDLqSw==" crossorigin="anonymous">
                    ^{pageHead pc}
                <body>
                    ^{pageBody pc}
            |]

getRootR :: Handler Html
getRootR = do
    -- unneeded return here to match README
    things' <- return [1..1142] :: Handler [Int]

    (things, widget) <- paginate 3 things'

    defaultLayout $ do
        setTitle "My title"
        [whamlet|$newline never
            <h1>Pagination
            <p>The things:
            <ul>
                $forall thing <- things
                    <li>Thing #{show thing}

            <div .pagination>
                ^{widget}
            |]

main :: IO ()
main = run 3000 =<< toWaiApp App

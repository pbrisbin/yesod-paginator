{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main
    ( main
    ) where

import Prelude

import Network.Wai.Handler.Warp (run)
import Yesod
import Yesod.Paginator

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
                    <div .container>
                        ^{pageBody pc}
            |]

getRootR :: Handler Html
getRootR = do
    let things' = [1 .. 1142] :: [Int]

    pages <- paginate 3 things'

    defaultLayout $ do
        setTitle "My title"
        [whamlet|$newline never
            <h1>Pagination Examples
            <h2>The things:
            <ul>
                $forall thing <- pageItems $ pagesCurrent pages
                    <li>Thing #{show thing}

            <h2>Simple navigation
            <div .pagination>
                ^{simple 10 pages}

            <h2>Ellipsed navigation
            <div .pagination>
                ^{ellipsed 10 pages}
            |]

main :: IO ()
main = run 3000 =<< toWaiApp App

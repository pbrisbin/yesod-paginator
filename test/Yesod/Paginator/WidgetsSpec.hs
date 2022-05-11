{-# LANGUAGE OverloadedStrings #-}
module Yesod.Paginator.WidgetsSpec
    ( spec
    ) where

import SpecHelper

spec :: Spec
spec = withApp $ do
    describe "simple" $ it "works" $ do
        get $ SimpleR 10 3 3

        statusIs 200
        bodyContains $ concat
            [ "<ul class=\"pagination\">"
            , "<li class=\"prev disabled\"><a>«</a></li>"
            , "<li class=\"active disabled\"><a>1</a></li>"
            , "<li class=\"next\"><a href=\"?p=2\">2</a></li>"
            , "<li class=\"next\"><a href=\"?p=3\">3</a></li>"
            , "<li class=\"next\"><a href=\"?p=2\">»</a></li>"
            , "</ul>"
            ]

        request $ do
            addGetParam "p" "3"
            setUrl $ SimpleR 10 3 3

        statusIs 200
        bodyContains $ concat
            [ "<ul class=\"pagination\">"
            , "<li class=\"prev\"><a href=\"?p=2\">«</a></li>"
            , "<li class=\"prev\"><a href=\"?p=2\">2</a></li>"
            , "<li class=\"active disabled\"><a>3</a></li>"
            , "<li class=\"next\"><a href=\"?p=4\">4</a></li>"
            , "<li class=\"next\"><a href=\"?p=4\">»</a></li>"
            , "</ul>"
            ]

        request $ do
            addGetParam "p" "4"
            setUrl $ SimpleR 10 3 3

        statusIs 200
        bodyContains $ concat
            [ "<ul class=\"pagination\">"
            , "<li class=\"prev\"><a href=\"?p=3\">«</a></li>"
            , "<li class=\"prev\"><a href=\"?p=2\">2</a></li>"
            , "<li class=\"prev\"><a href=\"?p=3\">3</a></li>"
            , "<li class=\"active disabled\"><a>4</a></li>"
            , "<li class=\"next disabled\"><a>»</a></li>"
            , "</ul>"
            ]

    describe "ellipsed" $ it "works" $ do
        get $ EllipsedR 10 3 3

        statusIs 200
        bodyContains $ concat
            [ "<ul class=\"pagination\">"
            , "<li class=\"prev disabled\"><a>«</a></li>"
            , "<li class=\"active disabled\"><a>1</a></li>"
            , "<li class=\"next\"><a href=\"?p=2\">2</a></li>"
            , "<li class=\"next\"><a href=\"?p=3\">3</a></li>"
            , "<li class=\"next\"><a href=\"?p=4\">4</a></li>"
            , "<li class=\"next\"><a href=\"?p=2\">»</a></li>"
            , "</ul>"
            ]

        get $ EllipsedR 15 3 3

        statusIs 200
        bodyContains $ concat
            [ "<ul class=\"pagination\">"
            , "<li class=\"prev disabled\"><a>«</a></li>"
            , "<li class=\"active disabled\"><a>1</a></li>"
            , "<li class=\"next\"><a href=\"?p=2\">2</a></li>"
            , "<li class=\"next\"><a href=\"?p=3\">3</a></li>"
            , "<li class=\"next disabled\"><a>…</a></li>"
            , "<li class=\"next\"><a href=\"?p=5\">5</a></li>"
            , "<li class=\"next\"><a href=\"?p=2\">»</a></li>"
            , "</ul>"
            ]

        request $ do
            addGetParam "p" "5"
            setUrl $ EllipsedR 15 3 3

        statusIs 200
        bodyContains $ concat
            [ "<ul class=\"pagination\">"
            , "<li class=\"prev\"><a href=\"?p=4\">«</a></li>"
            , "<li class=\"prev\"><a href=\"?p=1\">1</a></li>"
            , "<li class=\"prev disabled\"><a>…</a></li>"
            , "<li class=\"prev\"><a href=\"?p=3\">3</a></li>"
            , "<li class=\"prev\"><a href=\"?p=4\">4</a></li>"
            , "<li class=\"active disabled\"><a>5</a></li>"
            , "<li class=\"next disabled\"><a>»</a></li>"
            , "</ul>"
            ]

    describe "simple with page param" $ it "works" $ do
        get $ SimpleParamNameR 10 3 3 (PageParamName "page")

        statusIs 200
        bodyContains $ concat
            [ "<ul class=\"pagination\">"
            , "<li class=\"prev disabled\"><a>«</a></li>"
            , "<li class=\"active disabled\"><a>1</a></li>"
            , "<li class=\"next\"><a href=\"?page=2\">2</a></li>"
            , "<li class=\"next\"><a href=\"?page=3\">3</a></li>"
            , "<li class=\"next\"><a href=\"?page=2\">»</a></li>"
            , "</ul>"
            ]

        request $ do
            addGetParam "page" "3"
            setUrl $ SimpleParamNameR 10 3 3 (PageParamName "page")

        statusIs 200
        bodyContains $ concat
            [ "<ul class=\"pagination\">"
            , "<li class=\"prev\"><a href=\"?page=2\">«</a></li>"
            , "<li class=\"prev\"><a href=\"?page=2\">2</a></li>"
            , "<li class=\"active disabled\"><a>3</a></li>"
            , "<li class=\"next\"><a href=\"?page=4\">4</a></li>"
            , "<li class=\"next\"><a href=\"?page=4\">»</a></li>"
            , "</ul>"
            ]

        request $ do
            addGetParam "page" "4"
            setUrl $ SimpleParamNameR 10 3 3 (PageParamName "page")

        statusIs 200
        bodyContains $ concat
            [ "<ul class=\"pagination\">"
            , "<li class=\"prev\"><a href=\"?page=3\">«</a></li>"
            , "<li class=\"prev\"><a href=\"?page=2\">2</a></li>"
            , "<li class=\"prev\"><a href=\"?page=3\">3</a></li>"
            , "<li class=\"active disabled\"><a>4</a></li>"
            , "<li class=\"next disabled\"><a>»</a></li>"
            , "</ul>"
            ]

    describe "ellipsed with page param" $ it "works" $ do
        get $ EllipsedParamNameR 10 3 3 (PageParamName "page")

        statusIs 200
        bodyContains $ concat
            [ "<ul class=\"pagination\">"
            , "<li class=\"prev disabled\"><a>«</a></li>"
            , "<li class=\"active disabled\"><a>1</a></li>"
            , "<li class=\"next\"><a href=\"?page=2\">2</a></li>"
            , "<li class=\"next\"><a href=\"?page=3\">3</a></li>"
            , "<li class=\"next\"><a href=\"?page=4\">4</a></li>"
            , "<li class=\"next\"><a href=\"?page=2\">»</a></li>"
            , "</ul>"
            ]

        get $ EllipsedParamNameR 15 3 3 (PageParamName "page")

        statusIs 200
        bodyContains $ concat
            [ "<ul class=\"pagination\">"
            , "<li class=\"prev disabled\"><a>«</a></li>"
            , "<li class=\"active disabled\"><a>1</a></li>"
            , "<li class=\"next\"><a href=\"?page=2\">2</a></li>"
            , "<li class=\"next\"><a href=\"?page=3\">3</a></li>"
            , "<li class=\"next disabled\"><a>…</a></li>"
            , "<li class=\"next\"><a href=\"?page=5\">5</a></li>"
            , "<li class=\"next\"><a href=\"?page=2\">»</a></li>"
            , "</ul>"
            ]

        request $ do
            addGetParam "page" "5"
            setUrl $ EllipsedParamNameR 15 3 3 (PageParamName "page")

        statusIs 200
        bodyContains $ concat
            [ "<ul class=\"pagination\">"
            , "<li class=\"prev\"><a href=\"?page=4\">«</a></li>"
            , "<li class=\"prev\"><a href=\"?page=1\">1</a></li>"
            , "<li class=\"prev disabled\"><a>…</a></li>"
            , "<li class=\"prev\"><a href=\"?page=3\">3</a></li>"
            , "<li class=\"prev\"><a href=\"?page=4\">4</a></li>"
            , "<li class=\"active disabled\"><a>5</a></li>"
            , "<li class=\"next disabled\"><a>»</a></li>"
            , "</ul>"
            ]

    describe "filterParams" $ it "works" $ do
        let pageParamName = PageParamName "p"
            pageNumber :: Int
            pageNumber = 3
            params :: [(Text, Text)]
            params =
                [ ("p", "2")
                , ("p", "3")
                , ("foo", "bar")
                , ("ids[]", "1")
                , ("ids[]", "2")
                ]

        assertEq
            "filters page params not equal to the page number but keeps query params with the same name"
            (filterParams pageParamName pageNumber params)
            [("p", "3"), ("foo", "bar"), ("ids[]", "1"), ("ids[]", "2")]

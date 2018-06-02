{-# LANGUAGE OverloadedStrings #-}
module Yesod.Paginator.WidgetsSpec
    ( spec
    )
where

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

{-# LANGUAGE OverloadedStrings #-}
module Yesod.PaginatorSpec
    ( main
    , spec
    ) where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "paginated" $ do
        it "works" $ do
            get $ RootR 1142

            statusIs 200
            bodyContains $ concat $
                [ "<ul class=\"pagination\">"
                , "<li class=\"prev disabled\"><a>«</a></li>"
                , "<li class=\"active disabled\"><a>1</a></li>"
                , "<li class=\"next\"><a href=\"?p=2\">2</a></li>"
                , "<li class=\"next\"><a href=\"?p=3\">3</a></li>"
                , "<li class=\"next\"><a href=\"?p=4\">4</a></li>"
                , "<li class=\"next\"><a href=\"?p=5\">5</a></li>"
                , "<li class=\"next\"><a href=\"?p=6\">6</a></li>"
                , "<li class=\"next\"><a href=\"?p=7\">7</a></li>"
                , "<li class=\"next\"><a href=\"?p=8\">8</a></li>"
                , "<li class=\"next\"><a href=\"?p=9\">9</a></li>"
                , "<li class=\"next\"><a href=\"?p=10\">10</a></li>"
                , "<li class=\"next disabled\"><a>...</a></li>"
                , "<li class=\"next\"><a href=\"?p=381\">381</a></li>"
                , "<li class=\"next\"><a href=\"?p=2\">»</a></li>"
                , "</ul>"
                ]

            request $ do
                addGetParam "p" "88"
                setUrl $ RootR 1142

            statusIs 200
            bodyContains $ concat $
                [ "<ul class=\"pagination\">"
                , "<li class=\"prev\"><a href=\"?p=87\">«</a></li>"
                , "<li class=\"prev\"><a href=\"?p=1\">1</a></li>"
                , "<li class=\"prev disabled\"><a>...</a></li>"
                , "<li class=\"prev\"><a href=\"?p=79\">79</a></li>"
                , "<li class=\"prev\"><a href=\"?p=80\">80</a></li>"
                , "<li class=\"prev\"><a href=\"?p=81\">81</a></li>"
                , "<li class=\"prev\"><a href=\"?p=82\">82</a></li>"
                , "<li class=\"prev\"><a href=\"?p=83\">83</a></li>"
                , "<li class=\"prev\"><a href=\"?p=84\">84</a></li>"
                , "<li class=\"prev\"><a href=\"?p=85\">85</a></li>"
                , "<li class=\"prev\"><a href=\"?p=86\">86</a></li>"
                , "<li class=\"prev\"><a href=\"?p=87\">87</a></li>"
                , "<li class=\"active disabled\"><a>88</a></li>"
                , "<li class=\"next\"><a href=\"?p=89\">89</a></li>"
                , "<li class=\"next\"><a href=\"?p=90\">90</a></li>"
                , "<li class=\"next\"><a href=\"?p=91\">91</a></li>"
                , "<li class=\"next\"><a href=\"?p=92\">92</a></li>"
                , "<li class=\"next\"><a href=\"?p=93\">93</a></li>"
                , "<li class=\"next\"><a href=\"?p=94\">94</a></li>"
                , "<li class=\"next\"><a href=\"?p=95\">95</a></li>"
                , "<li class=\"next\"><a href=\"?p=96\">96</a></li>"
                , "<li class=\"next\"><a href=\"?p=97\">97</a></li>"
                , "<li class=\"next disabled\"><a>...</a></li>"
                , "<li class=\"next\"><a href=\"?p=381\">381</a></li>"
                , "<li class=\"next\"><a href=\"?p=89\">»</a></li>"
                , "</ul>"
                ]

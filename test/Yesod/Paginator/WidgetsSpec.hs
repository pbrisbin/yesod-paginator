{-# LANGUAGE OverloadedStrings #-}

module Yesod.Paginator.WidgetsSpec
  ( spec
  ) where

import Data.Functor ((<&>))
import SpecHelper
import Test.QuickCheck

spec :: Spec
spec = do
  withApp $ do
    describe "simple" $ it "works" $ do
      get $ SimpleR 10 3 3

      statusIs 200
      bodyContains
        $ concat
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
      bodyContains
        $ concat
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
      bodyContains
        $ concat
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
      bodyContains
        $ concat
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
      bodyContains
        $ concat
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
      bodyContains
        $ concat
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
      bodyContains
        $ concat
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
      bodyContains
        $ concat
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
      bodyContains
        $ concat
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
      bodyContains
        $ concat
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
      bodyContains
        $ concat
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
      bodyContains
        $ concat
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

  it "inserts" $ do
    let
      paramName = PageParamName "p"
      pageNumber = 1 :: Int
    setPageParameters paramName pageNumber [] `shouldBe` [("p", "1")]

  it "updates" $ do
    let
      paramName = PageParamName "p"
      pageNumber = 1 :: Int
    setPageParameters paramName pageNumber [("p", "foo")]
      `shouldBe` [("p", "1")]

  it "doesn't remove not-ours elements" $ property $ \(Params paramName pageNumber params) -> do
    let
      outputKeys =
        fst <$> setPageParameters paramName pageNumber params
      inputKeys = fst <$> params
     in
      all (`elem` outputKeys) inputKeys

  it "doesn't add not-ours elements" $ property $ \(Params paramName pageNumber params) -> do
    let
      outputKeys = fst <$> setPageParameters paramName pageNumber params
      inputKeys = fst <$> params
     in
      all (`elem` (unPageParamName paramName : inputKeys)) outputKeys

data Params = Params
  { paramsPageParamName :: PageParamName
  , paramsPageNumber :: Int
  , paramsParams :: [(Text, Text)]
  }
  deriving (Show)

instance Arbitrary Params where
  arbitrary = do
    params <- listOf $ liftArbitrary2 genText genText
    pageNumber <- getPositive <$> arbitrary
    pageParamName <- PageParamName <$> genText
    pure $ Params pageParamName pageNumber params
   where
    genText :: Gen Text
    genText = listOf (choose ('a', 'z')) <&> pack

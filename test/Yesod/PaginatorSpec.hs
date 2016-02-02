module Yesod.PaginatorSpec
    ( main
    , spec
    ) where

import Test.Hspec
--import Yesod.Paginator

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "paginated" $ do
    it "works" $ do
        True `shouldBe` True

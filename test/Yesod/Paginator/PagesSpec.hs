{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Yesod.Paginator.PagesSpec
    ( spec
    ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Functor.Classes (Eq1(..), Show1(..))
import Data.Proxy
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes
import Yesod.Paginator.Pages

instance Arbitrary a => Arbitrary (Page a) where
    arbitrary = (`toPage` 1) <$> arbitrary

instance Arbitrary a => Arbitrary (Pages a) where
    arbitrary = toPages 1 5 100 <$> arbitrary

instance Eq1 Page where
    liftEq f a b = liftEq f (pageItems a) (pageItems b)

instance Show1 Page where
    liftShowsPrec f g h = liftShowsPrec f g h . pageItems

instance Arbitrary1 Page where
    liftArbitrary = fmap (`toPage` 1) . liftArbitrary

instance Eq1 Pages where
    liftEq f a b = liftEq f (pagesCurrent a) (pagesCurrent b)

instance Show1 Pages where
    liftShowsPrec f g h = liftShowsPrec f g h . pagesCurrent

instance Arbitrary1 Pages where
    liftArbitrary = fmap (toPages 1 5 100) . liftArbitrary

spec :: Spec
spec = do
    describe "Page" $ do
        let proxyPage :: Proxy Page
            proxyPage = Proxy

        itPreserves $ functorLaws proxyPage
        itPreserves $ foldableLaws proxyPage
        itPreserves $ traversableLaws proxyPage

    describe "Pages" $ do
        let proxyPages :: Proxy Pages
            proxyPages = Proxy

        itPreserves $ functorLaws proxyPages
        itPreserves $ foldableLaws proxyPages
        itPreserves $ traversableLaws proxyPages

itPreserves :: Laws -> Spec
itPreserves Laws {..} = mkContext $ traverse_ (uncurry mkIt) lawsProperties
  where
    mkContext :: SpecWith a -> SpecWith a
    mkContext = context $ lawsTypeclass <> " laws"

    mkIt :: Example a => String -> a -> SpecWith (Arg a)
    mkIt name = it $ "preserves " <> name

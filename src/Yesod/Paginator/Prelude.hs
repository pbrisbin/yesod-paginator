{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Yesod.Paginator.Prelude
    ( module X
    , module Yesod.Paginator.Prelude
    )
where

import Prelude as X

import Control.Monad as X
import Data.List as X
import Data.Maybe as X
import Data.Semigroup as X ((<>))
import Data.Text as X (Text, pack, unpack)
import Numeric.Natural as X
import Safe as X

import Data.Function (on)
import Web.PathPieces

instance PathPiece Natural where
    toPathPiece = toPathPiece @Int . fromIntegral
    fromPathPiece p = do
        n <- fromPathPiece @Int p
        guard $ n >= 0
        pure $ fromIntegral n

tshow :: Show a => a -> Text
tshow = pack . show

nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = nubBy ((==) `on` f)

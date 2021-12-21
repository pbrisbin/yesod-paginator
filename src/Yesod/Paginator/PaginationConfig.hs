{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Paginator.PaginationConfig
    ( PaginationConfig(..)
    , PageParamName(..)
    , defaultPaginationConfig
    ) where

import Yesod.Paginator.Prelude

import Yesod.Core
import Yesod.Paginator.Pages

newtype PageParamName = PageParamName { unPageParamName :: Text }
    deriving stock Eq
    deriving newtype (Read, Show, PathPiece)

data PaginationConfig = PaginationConfig
    { paginationConfigPageParamName :: PageParamName
    , paginationConfigPerPage :: PerPage
    }

defaultPaginationConfig :: PaginationConfig
defaultPaginationConfig = PaginationConfig
    { paginationConfigPageParamName = PageParamName "p"
    , paginationConfigPerPage = 3
    }

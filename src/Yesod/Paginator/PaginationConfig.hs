{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Yesod.Paginator.PaginationConfig
    ( PaginationConfig(..)
    , PageParamName(..)
    , defaultPaginationConfig
    )
where

import Yesod.Core
import Yesod.Paginator.Pages
import Yesod.Paginator.Prelude

newtype PageParamName = PageParamName { unPageParamName :: Text }
    deriving (Eq)
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

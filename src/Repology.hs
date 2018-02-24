{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Repology where

import           Control.Category
import           Control.Error
import           Data.Aeson
import           Data.HashMap.Strict
import           Data.List
import           Data.Proxy
import           Data.Text (Text)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           GHC.Generics
import           Servant.API
import           Servant.Client

import qualified Data.Text as T
import qualified Data.Text.IO as T


baseUrl = BaseUrl Https "repology.org" 443 "/api/v1"

type Metapackage = Vector Package

compareMetapackage :: Metapackage -> Metapackage -> Ordering
compareMetapackage ps1 ps2 = compareMetapackage' (ps1 V.!? 0) (ps2 V.!? 0)
  where
    compareMetapackage' (Just p1) (Just p2) = compare (name p1) (name p2)
    compareMetapackage' Nothing (Just _) = LT
    compareMetapackage' (Just _) Nothing = GT
    compareMetapackage' _ _ = EQ

type Metapackages = HashMap Text Metapackage

type API =
  "metapackage" :>
  Capture "metapackage_name" Text :>
  Get '[JSON] Metapackage
  :<|>
  "metapackages" :>
  QueryParam "search" Text :>
  QueryParam "maintainer" Text :>
  QueryParam "category" Text :>
  QueryParam "inrepo" Text :>
  QueryParam "outdated" Bool :>
  QueryParam "notinrepo" Text :>
  QueryParam "minspread" Integer :>
  QueryParam "maxspread" Integer :>
  Get '[JSON] Metapackages
  :<|>
  "metapackages" :>
  Capture "name" Text :>
  QueryParam "search" Text :>
  QueryParam "maintainer" Text :>
  QueryParam "category" Text :>
  QueryParam "inrepo" Text :>
  QueryParam "outdated" Bool :>
  QueryParam "notinrepo" Text :>
  QueryParam "minspread" Integer :>
  QueryParam "maxspread" Integer :>
  Get '[JSON] Metapackages

data Package = Package
  { repo :: Text
  , name :: Text
  , version :: Text
  , origversion :: Maybe Text
  , status :: Maybe Text
  , summary :: Maybe Text
  , categories :: Maybe (Vector Text)
  , licenses :: Maybe (Vector Text)
  , maintainers :: Vector Text
  , www :: Maybe (Vector Text)
  , downloads :: Maybe (Vector Text)
  } deriving (Eq, Show, Generic, FromJSON)

api :: Proxy API
api = Proxy

metapackage :: Text -> ClientM (Vector Package)
metapackages ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  ClientM Metapackages
metapackages' ::
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  ClientM Metapackages


metapackage :<|> metapackages :<|> metapackages' = client api

-- type PagingResult = PagingResult (Vector Metapackage, ClientM PagingResult)

-- metapackages :: Text -> ClientM PagingResult
-- metapackages n = do
--   m <- ms n
--   return (lastMetapackageName m, sortedMetapackages m)

lastMetapackageName :: Metapackages -> Maybe Text
lastMetapackageName = keys >>> sort >>> Prelude.reverse >>> headMay

sortedMetapackages :: Metapackages -> Vector Metapackage
sortedMetapackages =
  elems >>>
  sortBy compareMetapackage >>>
  V.fromList

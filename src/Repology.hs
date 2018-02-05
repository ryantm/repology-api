{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Repology where

import           Control.Error
import           Data.Aeson
import           Data.HashMap.Lazy
import           Data.Proxy
import           Data.Text (Text)
import           Data.Vector
import           GHC.Generics
import           Servant.API
import           Servant.Client

import qualified Data.Text as T
import qualified Data.Text.IO as T

type API =
  "metapackage" :>
  Capture "metapackage_name" Text :>
  Get '[JSON] [Package]
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
  Get '[JSON] (HashMap Text [Package])

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
  } deriving (Eq, Show, Generic)

instance FromJSON Package


api :: Proxy API
api = Proxy

metapackage :: Text -> ClientM [Package]
metapackages ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  ClientM (HashMap Text [Package])

metapackage :<|> metapackages = client api

baseUrl = BaseUrl Https "repology.org" 443 "/api/v1"

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Repology where

import           Control.Error
import           Data.Aeson
import           Data.Proxy
import           Data.Vector
import           Data.Text (Text)
import           GHC.Generics
import           Servant.API
import           Servant.Client

import qualified Data.Text as T
import qualified Data.Text.IO as T

type API =
  "api" :>
  "v1" :>
  "metapackage" :>
  Capture "metapackage_name" Text :>
  Get '[JSON] [Package]

data Package = Package
  { repo :: Text
  , name :: Text
  , version :: Text
  , origversion :: Text
  , status :: Text
  , summary :: Text
  , categories :: Vector Text
  , licenses :: Vector Text
  , maintainers :: Vector Text
  , www :: Vector Text
  , downloads :: Vector Text
  } deriving (Eq, Show, Generic)

instance FromJSON Package


api :: Proxy API
api = Proxy

metapackage :: Text -> ClientM [Package]
metapackage = client api
{-# LANGUAGE DeriveGeneric #-}
module Jsons where

import Data.Aeson
import Data.Text
import GHC.Generics

data AccessToken = AccessToken { access_token :: Text
                               , token_type :: Text
                               , expires_in :: Int                                               
                               , refresh_token :: Text
                               } deriving (Generic, Show)

instance ToJSON AccessToken where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AccessToken where { }

data Error = Error { error :: Text
                   , error_description :: Text
                   } deriving (Generic, Show)

instance ToJSON Error where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Error where { }
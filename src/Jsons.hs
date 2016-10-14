{-# LANGUAGE DeriveGeneric #-}
module Jsons where

import Data.Aeson
import Data.Text
import GHC.Generics

data AccessTokenResponse = AccessTokenResponse { access_token :: Text
                                               , token_type :: Text
                                               , expires_in :: Int                                               
                                               , refresh_token :: Text
                                               } deriving (Generic, Show)

instance ToJSON AccessTokenResponse where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AccessTokenResponse where { }

data ErrorResponse = ErrorResponse { error :: Text
                                   , error_description :: Text
                                   } deriving (Generic, Show)

instance ToJSON ErrorResponse where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ErrorResponse where { }
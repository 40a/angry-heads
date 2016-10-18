{-# LANGUAGE DeriveGeneric #-}
module Jsons where

import Data.Aeson
import Data.Text
import GHC.Generics
import Control.Applicative ((<|>))

data AccessToken = AccessToken { access_token :: Text
                               , token_type :: Text
                               , expires_in :: Int
                               , refresh_token :: Text
                               } deriving (Generic, Show)

data Error = Error { error :: Text
                   , error_description :: Text
                   } deriving (Generic, Show)

data HHResult = HHSuccess AccessToken
              | HHError Error

instance ToJSON AccessToken where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AccessToken where { }

instance ToJSON Error where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Error where { }

instance FromJSON HHResult where
    parseJSON x =
        HHSuccess <$> parseJSON x
        <|>
        HHError <$> parseJSON x

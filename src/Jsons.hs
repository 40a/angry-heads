{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Jsons where

import Data.Aeson
import Data.Text
import GHC.Generics
import Control.Applicative ((<|>))
import Control.Monad (mzero)

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

data User = User { id :: Text
                 , first_name :: Text
                 , middle_name :: Text
                 , last_name :: Text
                 } deriving Show
                 
instance ToJSON User where
    toJSON (User _id _first_name _middle_name _last_name) = object ["id" .= _id
                                                                   ,"first_name" .= _first_name
                                                                   ,"middle_name" .= _middle_name
                                                                   ,"last_name" .= _last_name
                                                                   ]
    toEncoding (User _id _first_name _middle_name _last_name) =
        pairs ("id" .= _id
        --     <>"first_name" .= _first_name
        --     <>"middle_name" .= _middle_name
        --     <>"last_name" .: _last_name
              )
    
instance FromJSON User where
    parseJSON (Object value) = User <$>
                               value .: "id" <*>
                               value .: "first_name" <*>
                               value .: "middle_name" <*>
                               value .: "last_name"
    parseJSON _              = mzero

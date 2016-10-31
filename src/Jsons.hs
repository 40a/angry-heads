{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jsons where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text
import GHC.Generics

data AccessToken = AccessToken
    { access_token :: Text
    , token_type :: Text
    , expires_in :: Int
    , refresh_token :: Text
    } deriving (Generic, Show)

data Error = Error
    { error :: Text
    , error_description :: Text
    } deriving (Generic, Show)

data HHResult
    = HHSuccess AccessToken
    | HHError Error

instance ToJSON AccessToken where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AccessToken

instance ToJSON Error where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Error

instance FromJSON HHResult where
    parseJSON x = HHSuccess <$> parseJSON x <|> HHError <$> parseJSON x

data User = User
    { user_id :: Text
    , first_name :: Text
    , middle_name :: Text
    , last_name :: Text
    } deriving (Show)

instance ToJSON User where
    toJSON (User _user_id _first_name _middle_name _last_name) =
        object
            [ "id" .= _user_id
            , "first_name" .= _first_name
            , "middle_name" .= _middle_name
            , "last_name" .= _last_name
            ]
    toEncoding (User _user_id _first_name _middle_name _last_name) =
        pairs ("id" .= _user_id <> "first_name" .= _first_name <>
                "middle_name" .= _middle_name <>"last_name" .= _last_name)

instance FromJSON User where
    parseJSON (Object value) =
        User <$> value .: "id" <*> value .: "first_name" <*>
        value .: "middle_name" <*>
        value .: "last_name"
    parseJSON _ = mzero

data Company = Company
    { company_id :: Text
    , name :: Text
    , url :: Text
    } deriving (Show)

instance FromJSON Company where
    parseJSON (Object value) =
        Company <$> value .: "company_id" <*> value .: "company" <*>
        value .: "company_url"
    parseJSON _ = mzero

data Resume = Resume
    { experience :: [Company]
    } deriving (Show)

instance FromJSON Resume where
    parseJSON (Object value) =
        Resume <$> value .: "experience"
    parseJSON _ = mzero

data AllResumes = AllResumes
    { items :: [Resume]
    }

instance FromJSON AllResumes where
    parseJSON (Object value) =
        AllResumes <$> value .: "items"
    parseJSON _ = mzero

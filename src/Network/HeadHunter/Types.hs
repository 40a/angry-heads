{-# LANGUAGE OverloadedStrings #-}

module Network.HeadHunter.Types
    ( AuthToken(..)
    , AuthError(..)
    , AuthResult(..)
    , User(..)
    , Resumes(..)
    , Resume(..)
    , Company(..)
    ) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson
       (Value(Object), FromJSON(..), withArray, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Vector as V

data AuthToken = AuthToken
    { authAccessToken :: !Text
    , authTokenType :: !Text
    , authExpiresIn :: !Int
    , authRefreshToken :: !Text
    } deriving (Show)

data AuthError = AuthError
    { authError :: !Text
    , authErrorDescription :: !Text
    } deriving (Show)

data AuthResult
    = AuthSuccess !AuthToken
    | AuthFailure !AuthError

data User = User
    { userId :: !Text
    , userFirstName :: !Text
    , userMiddleName :: !Text
    , userLastName :: !Text
    } deriving (Show)

newtype Resumes = Resumes
    { resumes :: [Resume]
    } deriving (Show, Eq)

data Resume = Resume
    { resumeExperience :: [Company]
    } deriving (Show, Eq)

data Company = Company
    { companyId :: Maybe Text
    , companyName :: Text
    , companyUrl :: Text
    } deriving (Show, Eq)

instance FromJSON Resumes where
    parseJSON (Object o) = Resumes <$> (o .: "items" >>= asArrayOf "resume")
    parseJSON _ = mzero

instance FromJSON Resume where
    parseJSON (Object o) =
        Resume <$> (o .: "experience" >>= asArrayOf "companies")
    parseJSON _ = mzero

instance FromJSON Company where
    parseJSON (Object o) =
        Company <$> o .:? "company_id" <*> o .: "company" <*> o .: "company_url"
    parseJSON _ = mzero

instance FromJSON AuthToken where
    parseJSON (Object o) =
        AuthToken <$> o .: "access_token" <*> o .: "token_type" <*>
        o .: "expires_in" <*>
        o .: "refresh_token"
    parseJSON _ = mzero

instance FromJSON AuthError where
    parseJSON (Object o) =
        AuthError <$> o .: "error" <*> o .: "error_description"
    parseJSON _ = mzero

instance FromJSON AuthResult where
    parseJSON x = AuthSuccess <$> parseJSON x <|> AuthFailure <$> parseJSON x

instance FromJSON User where
    parseJSON (Object value) =
        User <$> value .: "id" <*> value .: "first_name" <*>
        value .: "middle_name" <*>
        value .: "last_name"
    parseJSON _ = mzero

asArrayOf
    :: FromJSON a
    => String -> Value -> Parser [a]
asArrayOf msg = withArray msg $ mapM parseJSON . V.toList

{-# LANGUAGE OverloadedStrings #-}

module Network.HeadHunter.Types where

import Control.Monad (mzero)
import Data.Aeson (Value(..), FromJSON(..), withArray, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Vector as V

newtype Resumes =
    Resumes [Resume]
    deriving (Show, Eq)

data Resume = Resume
    { experience :: [Company]
    } deriving (Show, Eq)

data Company = Company
    { companyId :: Maybe Text
    , name :: Text
    , url :: Text
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

asArrayOf
    :: FromJSON a
    => String -> Value -> Parser [a]
asArrayOf msg = withArray msg $ mapM parseJSON . V.toList

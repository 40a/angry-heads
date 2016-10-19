{-# LANGUAGE OverloadedStrings #-}
module Options (
    Options(..), Env(..),
    getOptions, getEnv
    ) where

import Turtle

data Options =
    Options
    { optionPort :: Int
    , optionDontCache :: Bool
    }

data Env =
    Env
    { envClientId :: Text
    , envClientSecret :: Text
    }

getOptions :: IO Options
getOptions =
    options "AngryHeads Server"
    $ Options
    <$> (optInt "port" 'p' "Port to serve on" <|> pure 8000)
    <*> (switch "no-cache" 'c' "Don't cache the static files" <|> pure False)

getEnv :: IO (Maybe Env)
getEnv =
    liftA2 Env
    <$> need "HH_OAUTH_CLIENT_ID"
    <*> need "HH_OAUTH_CLIENT_SECRET"

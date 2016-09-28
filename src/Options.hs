{-# LANGUAGE OverloadedStrings #-}
module Options (
    Options(..), Env(..),
    getOptions, getEnv
    ) where

import Turtle

data Options =
    Options
    { port :: Int
    , dontCache :: Bool
    }

data Env =
    Env
    { clientId :: Text
    , clientSecret :: Text
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

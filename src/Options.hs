{-# LANGUAGE OverloadedStrings #-}
module Options (Options(..), getOptions) where

import Turtle

data Options =
    Options
    { port :: Int
    , dontCache :: Bool
    }

getOptions :: IO Options
getOptions =
    options "AngryHeads Server"
    $ Options
    <$> (optInt "port" 'p' "Port to serve on" <|> pure 8000)
    <*> (switch "no-cache" 'c' "Don't cache the static files" <|> pure False)

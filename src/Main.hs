{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai
import Network.HTTP.Types (status200, hContentType)
import Network.Wai.Handler.Warp (run)
import Web.Scotty (scotty, get, text)

main :: IO ()
main = scotty 8000 $ do
    get "/" $ text "Hello World!"

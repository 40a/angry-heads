{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai
import Network.HTTP.Types (status200, hContentType)
import Network.Wai.Handler.Warp (run)

application :: Application
application _ respond = respond $
  responseLBS status200
              [(hContentType, "text/plain")]
              "Hello, world"

main :: IO ()
main = do
  putStrLn "Serving... (ctrl+c to break)"
  run 8000 application

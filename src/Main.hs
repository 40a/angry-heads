{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty (ScottyM, scottyApp, get, text)

import Options
import Static


app :: ScottyM ()
app =
    get "/hello" $ text "Hello World!!"


main :: IO ()
main = do
    Options {..} <- getOptions
    application <- scottyApp app
    withStatic <- makeWithStatic dontCache
    putStrLn $ "Server started! (port: " ++ show port ++ ", Ctrl+C to stop)"
    run port
        $ logStdout
        $ withStatic
        $ application

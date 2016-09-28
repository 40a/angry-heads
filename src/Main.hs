{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.Trans (lift)
import Data.Aeson (Value)
import Data.Text.Lazy (pack)
import Data.ByteString.Char8 (pack)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.HTTP.Conduit
import Network.HTTP.Simple
import System.Environment (getEnv)
import Web.Scotty (ScottyM, scottyApp, get, text, param)

import Options
import Static

app :: ScottyM ()
app = do
    get "/hello" $ text "Hello, world!"

    get "/oauth/hh" $ do
        code <- param "code"
        clientId <- lift $ getEnv "HeadHunterOAuthClientID"
        clientSecret <- lift $ getEnv "HeadHunterOAuthClientSecret"
        let request = urlEncodedBody [("grant_type", "authorization_code"),
                                      ("client_id", Data.ByteString.Char8.pack clientId),
                                      ("client_secret", Data.ByteString.Char8.pack clientSecret),
                                      ("code", Data.ByteString.Char8.pack code)]
                    $ parseRequest_ "POST https://hh.ru/oauth/token"
        (response :: Response Value) <- httpJSON request
        text $ Data.Text.Lazy.pack $ show (getResponseStatus response) ++ code

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

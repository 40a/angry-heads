{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Aeson (decode)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as BS8
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.HTTP.Conduit
import Network.HTTP.Simple (httpLBS, getResponseBody)
import Web.Scotty (ScottyM, scottyApp, get, text, param)

import Options
import Static
import Jsons (AccessToken, access_token, Error, error, error_description)

app :: Env -> ScottyM ()
app env = do
    get "/hello" $ text "Hello, world!"

    get "/oauth/hh" $ do
        code <- param "code"
        let Env {..} = env
        let request = urlEncodedBody
                    [ ("grant_type", "authorization_code")
                    , ("client_id", fromText clientId)
                    , ("client_secret", fromText clientSecret)
                    , ("code", BS8.pack code)
                    ]
                    $ parseRequest_ "POST https://hh.ru/oauth/token"
        response <- httpLBS request
        case (decode (getResponseBody response) :: Maybe AccessToken) of
          Nothing    -> case (decode (getResponseBody response) :: Maybe Error) of
                          Nothing    -> text "Нераспознаная ошибка"
                          Just value -> text . TL.pack $ (show (Jsons.error $ value) ++ " " ++ show (error_description $ value))
          Just value -> text . TL.pack $ show (access_token $ value) ++ " " ++ code
    where
        fromText = BS8.pack . T.unpack


main :: IO ()
main = do
    Options {..} <- getOptions
    getEnv >>= \case
        Just env -> do
            application <- scottyApp $ app env
            withStatic <- makeWithStatic dontCache
            putStrLn
                $ "Server started! (port: "
                ++ show port
                ++ ", Ctrl+C to stop)"
            run port
                $ logStdout
                $ withStatic
                $ application
        _ ->
           putStrLn "Environment wasn't configured properly!"

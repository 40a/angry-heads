{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Aeson (decode)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Blaze.ByteString.Builder as B
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.HTTP.Types (unauthorized401, badRequest400)
import Network.HTTP.Conduit
import Network.HTTP.Simple (httpLBS, getResponseBody)
import Web.Cookie
import Web.Scotty (ScottyM, scottyApp, get, text, param,
                   setHeader, redirect, ActionM, status)

import Options
import Static
import Jsons (HHResult(..))
import qualified Jsons

makeCookie :: BS.ByteString -> BS.ByteString -> SetCookie
makeCookie n v = def { setCookieName = n, setCookieValue = v }

renderSetCookie' :: SetCookie -> TL.Text
renderSetCookie' = TE.decodeUtf8 . B.toLazyByteString . renderSetCookie

setCookie :: BS.ByteString -> BS.ByteString -> ActionM ()
setCookie n v = setHeader "Set-Cookie" (renderSetCookie' (makeCookie n v))

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
        case decode . getResponseBody $ response of
            Just (HHSuccess t) -> do
                setCookie "access_token" $ fromText $ Jsons.access_token t <> "; path=/"
                redirect "/"
            Just (HHError e) -> do
                status unauthorized401
                setHeader "Lazy-Error-Message" (TL.pack . T.unpack $ (Jsons.error e))
--                text
--                    $ TL.pack . T.unpack
--                    $ Jsons.error e <> ": " <> Jsons.error_description e
            Nothing -> do
                status badRequest400
                setHeader "Lazy-Error-Message" "Нераспознанная ошибка"
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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Aeson (decode)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.ByteString.Char8 as BS8
import qualified Blaze.ByteString.Builder as B
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.HTTP.Types (unauthorized401, badRequest400)
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Simple (httpLBS, getResponseBody)
import Web.Cookie
import Web.Scotty (ScottyM, scottyApp, get, text, param,
                   setHeader, redirect, ActionM, status)

import Options
import Static
import Jsons (HHResult(..))
import qualified Jsons

app :: Env -> ScottyM ()
app env = do
    get "/hello" $ text "Hello, world!"

    get "/oauth/hh" $ do
        code <- param "code"
        let Env { envClientId = clientId
                , envClientSecret = clientSecret
                } = env
        let request = C.urlEncodedBody
                    [ ("grant_type", "authorization_code")
                    , ("client_id", fromText clientId)
                    , ("client_secret", fromText clientSecret)
                    , ("code", BS8.pack code)
                    ]
                    $ C.parseRequest_ "POST https://hh.ru/oauth/token"
        response <- httpLBS request
        case decode . getResponseBody $ response of
            Just (HHSuccess t) -> do
                setCookie . accessTokenCookie $ Jsons.access_token t
                redirect "/"
            Just (HHError e) -> do
                status unauthorized401
                setHeader "Lazy-Error-Message" .
                    TL.pack . T.unpack $ Jsons.error e
            Nothing -> do
                status badRequest400
                setHeader "Lazy-Error-Message" "Нераспознанная ошибка"
    where
        fromText = BS8.pack . T.unpack


main :: IO ()
main = do
    Options { optionPort = port
            , optionDontCache = dontCache
            } <- getOptions
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

setCookie :: SetCookie -> ActionM ()
setCookie =
    setHeader "Set-Cookie" . TE.decodeUtf8 .
    B.toLazyByteString . renderSetCookie

accessTokenCookie :: T.Text -> SetCookie
accessTokenCookie value =
    def { setCookieName = "access_token"
        , setCookieValue = BS8.pack . T.unpack $ value
        , setCookiePath = Just "/"
        }

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AngryHeads where

import qualified Blaze.ByteString.Builder as B
import Control.Monad.Trans (liftIO)
import Data.Aeson (decode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Simple
       (httpLBS, getResponseBody, setRequestHeaders)
import Network.HTTP.Types
       (ok200, unauthorized401, badRequest400, hAuthorization, hUserAgent)
import Web.Cookie
import Web.Scotty
       (ScottyM, get, text, param, setHeader, redirect, ActionM, status)
import Web.Scotty.Cookie hiding (setCookie)

import Network.HeadHunter.Types (AuthResult(..))
import qualified Network.HeadHunter.Types as HH
import Options

app :: Env -> ScottyM ()
app env = do
    get "/api/v1/hello" $ text "{ \"message\": \"Hello, world!\" }"
    get "/api/v1/users/current" $ do
        accessToken <- getCookie "access_token"
        case accessToken of
            Just accessToken' -> do
                let request =
                        setRequestHeaders
                            [ (hAuthorization, makeAuthorization accessToken')
                            , (hUserAgent, userAgent)
                            ] $
                        C.parseRequest_ "GET https://api.hh.ru/me"
                response <- httpLBS request
                liftIO . BSL.putStrLn $ getResponseBody response
                case decode . getResponseBody $ response of
                    Just user -> do
                        status ok200
                        text (TL.pack . T.unpack $ HH.userId user)
                    Nothing -> do
                        status ok200
                        text "null"
            Nothing -> do
                status unauthorized401
                text "null"
    get "/api/v1/users/current/companies" $ do
        accessToken <- getCookie "access_token"
        case accessToken of
            Just accessToken' -> do
                let request =
                        setRequestHeaders
                            [ (hAuthorization, makeAuthorization accessToken')
                            , (hUserAgent, userAgent)
                            ] $
                        C.parseRequest_ "GET https://api.hh.ru/resumes/mine"
                response <- httpLBS request
                liftIO . BSL.putStrLn $ getResponseBody response
                case decode . getResponseBody $ response of
                    Just allResumes -> do
                        let firstResume = head $ HH.resumes allResumes
                        let firstCompany =
                                head $ HH.resumeExperience firstResume
                        status ok200
                        text . TL.pack . T.unpack $ HH.companyName firstCompany
                    Nothing -> do
                        status ok200
                        text "null"
            Nothing -> do
                status unauthorized401
                text "null"
    get "/oauth/hh" $ do
        code <- param "code"
        let Env {envClientId = clientId, envClientSecret = clientSecret} = env
        let request =
                C.urlEncodedBody
                    [ ("grant_type", "authorization_code")
                    , ("client_id", fromText clientId)
                    , ("client_secret", fromText clientSecret)
                    , ("code", BS8.pack code)
                    ] $
                C.parseRequest_ "POST https://hh.ru/oauth/token"
        response <- httpLBS request
        case decode . getResponseBody $ response of
            Just (AuthSuccess t) -> do
                setCookie . accessTokenCookie $ HH.authAccessToken t
                redirect "/"
            Just (AuthFailure e) -> do
                status unauthorized401
                setHeader "Lazy-Error-Message" . TL.pack . T.unpack $
                    HH.authError e
            Nothing -> do
                status badRequest400
                setHeader "Lazy-Error-Message" "Нераспознанная ошибка"
  where
    fromText = BS8.pack . T.unpack
    userAgent =
        BS8.pack "AngryHeads/1.0 (https://github.com/progmsk/angry-heads)"
    makeAuthorization token = BS8.pack $ "Bearer " ++ T.unpack token

setCookie :: SetCookie -> ActionM ()
setCookie =
    setHeader "Set-Cookie" .
    TE.decodeUtf8 . B.toLazyByteString . renderSetCookie

accessTokenCookie :: T.Text -> SetCookie
accessTokenCookie value =
    def
    { setCookieName = "access_token"
    , setCookieValue = BS8.pack . T.unpack $ value
    , setCookiePath = Just "/"
    }

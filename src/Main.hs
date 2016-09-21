{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (isPrefixOf)
import Network.HTTP.Types.Header
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static (
    CachingStrategy (CustomCaching), FileMeta (fm_fileName),
    addBase, hasPrefix, hasSuffix, initCaching, only,
    staticPolicy', (<|>), (>->)
    )
import Web.Scotty (ScottyM, scottyApp, get, text)


app :: ScottyM ()
app =
    get "/hello" $ text "Hello World!!"


main :: IO ()
main = do
    cache <- initCaching . CustomCaching $ noCacheLocalJS  -- TODO: should be configurable
    application <- scottyApp app
    putStrLn "Started! (hit Ctrl+C to stop)"
    run 8000
        $ logStdout
        $ staticPolicy' cache policy
        $ application
  where
    policy =
        ( only [("", "index.html")]
          <|> hasPrefix "css/"
          <|> hasPrefix "js/"
          <|> hasSuffix ".html"
        ) >-> addBase "static"

    -- this rule disables an any possible caching for local .js-files
    -- (for the developer's needs)
    noCacheLocalJS fm
        | "static/js/" `isPrefixOf` fm_fileName fm =
            [ (hPragma, "no-cache")
            , (hExpires, "0")
            , (hCacheControl, "no-cache, no-store, must-revalidate")
            ]
        | otherwise = []

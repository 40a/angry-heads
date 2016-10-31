{-# LANGUAGE LambdaCase #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty (scottyApp)

import AngryHeads
import Options
import Static

main :: IO ()
main = do
    Options {optionPort = port, optionDontCache = dontCache} <- getOptions
    getEnv >>= \case
        Just env -> do
            application <- scottyApp $ app env
            withStatic <- makeWithStatic dontCache
            putStrLn $
                "Server started! (port: " ++ show port ++ ", Ctrl+C to stop)"
            run port . logStdout . withStatic $ application
        _ -> putStrLn "Environment wasn't configured properly!"

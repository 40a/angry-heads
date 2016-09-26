{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Cross-platform Make replacement :)

import Control.Concurrent (threadDelay)
import Data.List (isSuffixOf)
import Prelude hiding (FilePath)
import System.FSNotify (Event(Modified), withManager, watchTree)
import Turtle hiding (within)


type Action = IO ExitCode

data BuildMode
    = BuildOnce
    | AutoRebuild

data Command
    = Client BuildMode
    | Server
    | All


main :: IO ()
main =
    getCommand >>= execute >>= exit
  where
    execute = \case
        Client BuildOnce ->
            buildClient

        Client AutoRebuild ->
            autoRebuildClient

        Server ->
            buildServer

        All ->
            buildClient .&&. buildServer


buildClient :: Action
buildClient = do
    echo "Building: client..................."
    within "client"
        $ proc "elm-make"
               [ "src/Main.elm"
               , "--warn", "--yes"
               , "--output", "../static/js/app.js"
               ] empty


autoRebuildClient :: Action
autoRebuildClient = return ExitSuccess <* do
    echo "Client auto-rebuild: watching for changes (hit Ctrl-C to stop)..."

    withManager $ \mgr -> do
        void $ watchTree
            mgr
            "./client/src"
            (\case
              Modified path _ -> ".elm" `isSuffixOf` path
              _ -> False
            )
            (const $ void buildClient)
        forever . threadDelay $ 1000000  -- 1sec


buildServer :: Action
buildServer = do
    echo "Building: server..................."
    proc "stack" [ "build"
                 , "angry-heads:exe:server"
                 ] empty

-- Options

getCommand :: IO Command
getCommand =
    options "BuildIt, an awesome build tool!"
    ( subcommand "client" "Build a client"
      ( Client . toBuildMode
        <$> switch "watch" 'w' "Stay watching for changes (auto rebuild)"
      )
      <|> subcommand "server" "Build a server" (pure Server)
      <|> pure All
    )
  where
    toBuildMode flag
        | flag = AutoRebuild
        | otherwise = BuildOnce


-- Helpers

-- | Executes the body within targetDir
within :: FilePath -> IO a -> IO a
within targetDir body = do
    oldDir <- pwd
    cd targetDir
    result <- body
    cd oldDir
    return result

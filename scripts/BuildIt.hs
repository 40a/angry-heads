{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Cross-platform Make replacement :)

import Turtle hiding (within)


data Target = Client | Server | All


within :: Turtle.FilePath -> IO a -> IO a
within targetDir body = do
    oldDir <- pwd
    cd targetDir
    result <- body
    cd oldDir
    return result


buildClient :: IO ExitCode
buildClient = do
    echo "Building: client..................."
    within "client"
        $ shell "elm-make src/Main.elm --yes --output ../static/js/app.js" empty


buildServer :: IO ExitCode
buildServer = do
    echo "Building: server..................."
    shell "stack build angry-heads:exe:server" empty


main :: IO ()
main =
    options "BuildIt, an awesome build tool!"
    ( subcommand "client" "Build a client" (pure Client)
      <|> subcommand "server" "Build a server" (pure Server)
      <|> pure All
    )
    >>= \case
        Client -> buildClient
        Server -> buildServer
        _ -> buildClient .&&. buildServer
    >>= exit

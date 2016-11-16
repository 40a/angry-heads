module Main exposing (..)

import Html.App exposing (program)
import Platform exposing (Program)

import Model exposing (..)
import View exposing (view)
import Update exposing (update)

main : Program Never
main =
  program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

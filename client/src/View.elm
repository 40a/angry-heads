module View exposing (view)

import Html exposing (..)

import Model exposing (..)

view : Model -> Html Msg
view m = h1 [] [text m]

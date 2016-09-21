module Main exposing (..)

import Html.App exposing (beginnerProgram)
import Platform exposing (Program)

import Model exposing (initialState)
import View exposing (view)
import Update exposing (update)

main : Program Never
main =
  beginnerProgram
  { model = initialState
  , view = view
  , update = update
  }

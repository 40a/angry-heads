module Update exposing (update)

import Model exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ToggleNav -> ( { model | menuIsVisible = not model.menuIsVisible }, Cmd.none)

    Authenticate -> (model, authenticate)
  
    FetchSucceed authenticatedUser -> ( { model | user = Just authenticatedUser }, Cmd.none)
    
    FetchFail _ -> ( { model | user = Nothing }, Cmd.none)

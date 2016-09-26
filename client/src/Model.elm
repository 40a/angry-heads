module Model exposing (..)


type alias Model =
    { menuIsVisible : Bool
    , message : String
    }


type Msg
    = ToggleNav


initialState : Model
initialState =
    { menuIsVisible = False
    , message = "Hello world!"
    }

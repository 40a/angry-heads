module Update exposing (update)

import Model exposing (..)


update : Msg -> Model -> Model
update ToggleNav m =
    { m | menuIsVisible = not m.menuIsVisible }

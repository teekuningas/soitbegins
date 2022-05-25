module States.Termination exposing (init, view)

import Html exposing (Html, div, text)


type alias InitData =
    { message : String }


init : String -> ( InitData, () )
init message =
    ( { message = message }, () )


view : String -> Html msg
view message =
    div [] [ text message ]

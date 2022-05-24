module States.Termination exposing (init, view)

import Html exposing (Html, div, text)


init : String -> ( { message : String }, () )
init message =
    ( { message = message }, () )


view : String -> Html msg
view message =
    div [] [ text message ]

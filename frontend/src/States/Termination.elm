module States.Termination exposing (init, view)

import Html exposing (Html, div, text)


init : String -> ( { message : String }, () )
init message =
    ( { message = message }, () )


view : { message : String } -> Html msg
view values =
    div [] [ text values.message ]

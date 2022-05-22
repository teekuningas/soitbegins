module States.Termination exposing (Msg, subscriptions, update, view, init)

import Html exposing (Html, div, text)
import Platform.Sub


type Msg
    = EmptyMsg


init : String -> ( String, Cmd Msg )
init message =
    ( message, Cmd.none )


subscriptions : String -> Sub Msg
subscriptions message =
    Platform.Sub.none


view : String -> Html Msg
view message =
    div [] [ text message ]


update : Msg -> String -> ( String, Cmd Msg )
update msg message =
    ( message, Cmd.none )


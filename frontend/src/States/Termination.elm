module States.Termination exposing (Msg, subscriptions, update, view)

import Common exposing (Model(..))
import Html exposing (Html, div, text)
import Platform.Sub


type Msg
    = EmptyMsg


subscriptions : String -> Sub Msg
subscriptions message =
    Platform.Sub.none


view : String -> Html Msg
view message =
    div [] [ text message ]


update : Msg -> String -> ( Model, Cmd Msg )
update msg message =
    ( Termination message, Cmd.none )

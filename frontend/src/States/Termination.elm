module States.Termination exposing (Msg, subscriptions, update, view)

import Html exposing (Html, div, text)
import Model.Model exposing (Model(..))
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

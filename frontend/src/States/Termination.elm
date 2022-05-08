module States.Termination exposing (Msg, subscriptions, view, update)

import Html exposing (div, text, Html)
import Platform.Sub

import Common exposing (Model(..))


type Msg = EmptyMsg


subscriptions : String -> Sub Msg
subscriptions message = 
    Platform.Sub.none


view : String -> Html Msg
view message = div [] [ text message ]


update : Msg -> String -> ( Model, Cmd Msg )
update msg message = 
    ( Termination message, Cmd.none )

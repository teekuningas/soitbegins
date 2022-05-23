module States.MainMenu exposing (Msg(..), subscriptions, update, view)

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onResize)
import Communication.Types exposing (User)
import HUD.Page exposing (embedInCanvas)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Platform.Sub
import Task
import WebGL exposing (Mesh)
import World.Types exposing (MeshList, Vertex)


type Msg
    = ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | StartGameMsg
    | TransitionToInGameLoaderMsg


subscriptions : Sub Msg
subscriptions =
    Platform.Sub.batch
        [ onResize (\width height -> ResizeMsg)
        ]


view : { user : User } -> Html Msg
view values =
    embedInCanvas
        []
        [ div
            [ class "main-menu-container" ]
            [ p [] [ text "So it begins (the grand hot air balloon adventure)" ]
            , p [] [ text (String.append "Welcome " values.user.name) ]
            , button [ onClick StartGameMsg ] [ text "Start here" ]
            ]
        ]
        []
        []


update : Msg -> ( (), Cmd Msg )
update msg =
    case msg of
        StartGameMsg ->
            ( ()
            , Task.perform (always TransitionToInGameLoaderMsg) (Task.succeed ())
            )

        ResizeMsg ->
            ( ()
            , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
            )

        _ ->
            ( ()
            , Cmd.none
            )

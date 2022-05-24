module States.GatherInfo exposing (Msg(..), init, subscriptions, update, view)

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onResize)
import Communication.Types exposing (User)
import HUD.Page exposing (embedInCanvas)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Platform.Sub
import Task
import WebGL exposing (Mesh)
import World.Types exposing (Data, MeshList, Vertex)


type Msg
    = ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ContinueMsg
    | NameUpdatedMsg String
    | TransitionToMainMenuMsg


init : { earthMesh : Mesh Vertex, serverUpdateInterval : Int } -> ( { data : Data, user : User }, Cmd Msg )
init transitionData =
    let
        data =
            { earthMesh = transitionData.earthMesh
            , serverUpdateInterval = transitionData.serverUpdateInterval
            }

        user =
            { name = ""
            }
    in
    ( { data = data, user = user }
    , Cmd.none
    )


subscriptions : Sub Msg
subscriptions =
    Platform.Sub.batch
        [ onResize (\width height -> ResizeMsg)
        ]


view : User -> Html Msg
view user =
    embedInCanvas
        []
        [ div
            [ class "gather-info-container" ]
            [ p [] [ text "So it begins (the grand hot air balloon adventure)" ]
            , input
                [ placeholder "Please write your name here.."
                , value user.name
                , onInput NameUpdatedMsg
                ]
                []
            , button [ onClick ContinueMsg ] [ text "And continue!" ]
            ]
        ]
        []
        []


update : Msg -> { user : User } -> ( { user : User }, Cmd Msg )
update msg values =
    case msg of
        ContinueMsg ->
            ( values
            , if values.user.name /= "" then
                Task.perform (always TransitionToMainMenuMsg) (Task.succeed ())

              else
                Cmd.none
            )

        NameUpdatedMsg newName ->
            let
                user =
                    { name = newName }
            in
            ( { values | user = user }
            , Cmd.none
            )

        -- Refresh canvas
        ResizeMsg ->
            ( values, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

        _ ->
            ( values
            , Cmd.none
            )

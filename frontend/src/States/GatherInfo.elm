module States.GatherInfo exposing (Msg(..), subscriptions, update, view, init)

import World.Types exposing (Vertex, MeshList)
import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onResize)
import HUD.Page exposing (embedInCanvas)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Platform.Sub
import Task
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)

import States.GatherInfoTypes exposing (GatherInfoData)
import States.MainMenuTypes exposing (MenuData)


type Msg
    = ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ContinueMsg
    | NameUpdatedMsg String
    | TransitionToMainMenuMsg MenuData


init : GatherInfoData -> ( GatherInfoData, Cmd Msg )
init gatherInfoData = 
    (gatherInfoData, Cmd.none)


subscriptions : GatherInfoData -> Sub Msg
subscriptions gatherInfoData =
    Platform.Sub.batch
        [ onResize (\width height -> ResizeMsg)
        ]


view : GatherInfoData -> Html Msg
view gatherInfoData =
    embedInCanvas
        []
        [ div
            [ class "gather-info-container" ]
            [ p [] [ text "So it begins (the grand hot air balloon adventure)" ]
            , input
                [ placeholder "Please write your name here.."
                , value gatherInfoData.user.name
                , onInput NameUpdatedMsg
                ]
                []
            , button [ onClick ContinueMsg ] [ text "And continue!" ]
            ]
        ]
        []
        []


update : Msg -> GatherInfoData -> ( GatherInfoData, Cmd Msg )
update msg gatherInfoData =
    case msg of
        ContinueMsg ->
            if gatherInfoData.user.name /= "" then
                let
                    menuData =
                        { earthMesh = gatherInfoData.earthMesh
                        , canvasDimensions = gatherInfoData.canvasDimensions
                        , user = gatherInfoData.user
                        }
                in
                ( gatherInfoData
                , Task.perform (always (TransitionToMainMenuMsg menuData)) (Task.succeed ())
                )

            else
                ( gatherInfoData
                , Cmd.none
                )

        NameUpdatedMsg newName ->
            let
                user =
                    { name = newName }
            in
            ( { gatherInfoData | user = user }
            , Cmd.none
            )

        ResizeMsg ->
            ( gatherInfoData, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

        ViewportMsg returnValue ->
            let
                newCanvasDimensions =
                    returnValue
                        |> Result.map .viewport
                        |> Result.map
                            (\v ->
                                { width = round v.width
                                , height = round v.height
                                }
                            )
                        |> Result.withDefault gatherInfoData.canvasDimensions

                newGatherInfoData =
                    { gatherInfoData | canvasDimensions = newCanvasDimensions }
            in
            ( newGatherInfoData
            , Cmd.none
            )
        TransitionToMainMenuMsg _ ->
            ( gatherInfoData
            , Cmd.none 
            )



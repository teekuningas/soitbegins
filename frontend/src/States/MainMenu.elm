module States.MainMenu exposing (Msg(..), subscriptions, update, view, init)

import World.Types exposing (Vertex, MeshList)
import States.InGameLoader
import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onResize)
import HUD.Page exposing (embedInCanvas)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Platform.Sub
import Task
import WebGL exposing (Mesh)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import States.MainMenuTypes exposing (MenuData)
import States.InGameLoaderTypes exposing (GameLoaderData)


type Msg
    = ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | StartGameMsg
    | TransitionToInGameLoaderMsg GameLoaderData


init : MenuData -> ( MenuData, Cmd Msg )
init menuData =
    (menuData, Cmd.none)


subscriptions : MenuData -> Sub Msg
subscriptions menuData =
    Platform.Sub.batch
        [ onResize (\width height -> ResizeMsg)
        ]


view : MenuData -> Html Msg
view menuData =
    embedInCanvas
        []
        [ div
            [ class "main-menu-container" ]
            [ p [] [ text "So it begins (the grand hot air balloon adventure)" ]
            , p [] [ text (String.append "Welcome " menuData.user.name) ]
            , button [ onClick StartGameMsg ] [ text "Start here" ]
            ]
        ]
        []
        []


update : Msg -> MenuData -> ( MenuData, Cmd Msg )
update msg menuData =
    case msg of
        StartGameMsg ->
            let
                gameLoaderData =
                    { earthMesh = menuData.earthMesh
                    , renderData = Nothing
                    , connectionData = Nothing
                    , earth = Nothing
                    , canvasDimensions = menuData.canvasDimensions
                    , user = menuData.user
                    , hero = Nothing
                    }
            in
            ( menuData
            , Task.perform (always (TransitionToInGameLoaderMsg gameLoaderData)) (Task.succeed ())
            )

        ResizeMsg ->
            ( menuData, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

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
                        |> Result.withDefault menuData.canvasDimensions

                newMenuData =
                    { menuData | canvasDimensions = newCanvasDimensions }
            in
            ( newMenuData
            , Cmd.none
            )
        TransitionToInGameLoaderMsg _ ->
            ( menuData
            , Cmd.none
            )


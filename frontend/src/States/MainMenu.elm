module States.MainMenu exposing (Msg, subscriptions, update, view)

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onResize)
import HUD.Page exposing (embedInCanvas)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Model.Model exposing (MenuData, Model(..))
import Platform.Sub
import Task


type Msg
    = ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | StartGameMsg


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
            , button [ onClick StartGameMsg ] [ text "Start here" ]
            ]
        ]
        []
        []


update : Msg -> MenuData -> ( Model, Cmd Msg )
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
                    }
            in
            ( InGameLoader gameLoaderData
            , Cmd.none
            )

        ResizeMsg ->
            ( MainMenu menuData, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

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
            ( MainMenu newMenuData
            , Cmd.none
            )

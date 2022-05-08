module States.MainMenu exposing (Msg, subscriptions, view, update)

import Html exposing (div, text, button, p, Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Platform.Sub

import Task

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onResize)

import Page exposing (embedInCanvas)

import Common exposing (Model(..), MenuData)


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
        [ div
            [ class "main-menu-container" ]
            [ p [] [ text "So it begins (the grand hot air balloon adventure)" ]
            , button [ onClick StartGameMsg ] [ text "Start here" ] ]
        ] [] []


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
                    , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
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



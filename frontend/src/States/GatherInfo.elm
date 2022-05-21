module States.GatherInfo exposing (Msg, subscriptions, update, view)

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onResize)
import HUD.Page exposing (embedInCanvas)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Model.Model exposing (GatherInfoData, Model(..))
import Platform.Sub
import Task


type Msg
    = ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ContinueMsg
    | NameUpdatedMsg String


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
                [ placeholder "Please write your name here first.."
                , value gatherInfoData.user.name
                , onInput NameUpdatedMsg
                ]
                []
            , button [ onClick ContinueMsg ] [ text "And continue!" ]
            ]
        ]
        []
        []


update : Msg -> GatherInfoData -> ( Model, Cmd Msg )
update msg gatherInfoData =
    case msg of
        ContinueMsg ->
            if gatherInfoData.user.name /= "" then
                let
                    newMenuData =
                        { earthMesh = gatherInfoData.earthMesh
                        , canvasDimensions = gatherInfoData.canvasDimensions
                        , user = gatherInfoData.user
                        }
                in
                ( MainMenu newMenuData
                , Cmd.none
                )

            else
                ( GatherInfo gatherInfoData
                , Cmd.none
                )

        NameUpdatedMsg newName ->
            let
                user =
                    { name = newName }
            in
            ( GatherInfo { gatherInfoData | user = user }
            , Cmd.none
            )

        ResizeMsg ->
            ( GatherInfo gatherInfoData, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

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
            ( GatherInfo newGatherInfoData
            , Cmd.none
            )

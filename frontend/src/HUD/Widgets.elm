module HUD.Widgets exposing (fpsOverlay, overviewToggleOverlay, Msg(..))

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onMouseDown)
import Model.Model exposing (RenderData)


type Msg = OverviewToggleMsg


fpsOverlay : RenderData -> Html Msg
fpsOverlay renderData =
    let
        fps =
            round (1000 / (renderData.elapsed - renderData.previousElapsed))
                |> String.fromInt
    in
    div
        [ id "fps-overlay" ]
        [ span
            []
            [ text ("FPS: " ++ fps)
            ]
        ]

overviewToggleOverlay : Bool -> Html Msg
overviewToggleOverlay isOn =
    let
        className = if isOn then "overview-toggle-on" else "overview-toggle-off"
    in
    div
        [ id "overview-toggle-overlay" 
        , class (String.append className " noselect")
        , onMouseDown OverviewToggleMsg
        ]
        [ div 
            [ ] 
            [ text "Overview" ]
        ] 

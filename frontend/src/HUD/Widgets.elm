module HUD.Widgets exposing (fpsOverlay, overviewToggleOverlay, debugOverlay, Msg(..))

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onMouseDown)
import Model.Model exposing (RenderData, GameData)


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

debugOverlay : GameData -> Html Msg
debugOverlay gameData =
    let
        message = ""
        -- (Debug.toString gameData.earth)
        -- |> String.append (Debug.toString gameData.connectionData.earth)
    in
    div
        [ id "debug-overlay" 
        , class "noselect"
        ]
        [ div 
            [ ] 
            [ text message ]
        ] 

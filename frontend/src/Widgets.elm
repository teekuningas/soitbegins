module Widgets exposing (fpsOverlay)

import Common exposing (RenderData)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (id)


fpsOverlay : RenderData -> Html msg
fpsOverlay renderData =
    let
        fpsFun previous =
            round (1000 / (renderData.elapsed - previous))

        fps =
            Maybe.map fpsFun renderData.previousElapsed
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""
    in
    div
        [ id "fps-overlay" ]
        [ span
            []
            [ text ("FPS: " ++ fps)
            ]
        ]

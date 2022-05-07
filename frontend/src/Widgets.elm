module Widgets exposing (fpsOverlay)

import Common exposing (RenderData)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (id)


fpsOverlay : RenderData -> Html msg
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

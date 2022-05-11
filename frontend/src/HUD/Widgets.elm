module HUD.Widgets exposing (fpsOverlay)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (id)
import Model.Model exposing (RenderData)


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

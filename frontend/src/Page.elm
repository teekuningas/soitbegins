module Page exposing (embedInCanvas)

import Common exposing (viewportSize)
import Html exposing (Html, div)
import Html.Attributes exposing (height, id, style, width)
import WebGL


embedInCanvas : List (Html msg) -> List (Html.Attribute msg) -> List WebGL.Entity -> Html msg
embedInCanvas outer attr inner =
    div
        [ id "canvas-container" ]
        (outer
            ++ [ WebGL.toHtml
                    ([ width (Tuple.first viewportSize)
                     , height (Tuple.second viewportSize)
                     , style "display" "block"
                     , style "height" "100vh"
                     , style "width" "100vw"
                     , id "webgl-canvas"
                     ]
                        ++ attr
                    )
                    inner
               ]
        )

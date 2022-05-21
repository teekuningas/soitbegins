module HUD.Page exposing (embedInCanvas, viewportSize)

import Html exposing (Html, div)
import Html.Attributes exposing (height, id, style, width)
import WebGL


viewportSize : ( Int, Int )
viewportSize =
    ( 800, 800 )


embedInCanvas : List(Html.Attribute msg) -> List (Html msg) -> List (Html.Attribute msg) -> List WebGL.Entity -> Html msg
embedInCanvas containerAttrs outer attrs inner =
    div
        ([ id "canvas-container" ] ++ containerAttrs)
        (outer
            ++ [ WebGL.toHtml
                    ([ width (Tuple.first viewportSize)
                     , height (Tuple.second viewportSize)
                     , style "display" "block"
                     , style "height" "100vh"
                     , style "width" "100vw"
                     , id "webgl-canvas"
                     ]
                        ++ attrs
                    )
                    inner
               ]
        )

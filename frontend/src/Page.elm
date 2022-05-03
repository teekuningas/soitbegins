module Page exposing (embedInCanvas)

import WebGL
import Html exposing (Html, div)
import Html.Attributes exposing (width, height, style, id)

import Common exposing (viewportSize)


embedInCanvas : List (Html msg) -> List (Html.Attribute msg) -> List WebGL.Entity -> Html msg
embedInCanvas outer attr inner  = 
      div
        [ id "canvas-container" ]
        (outer ++ [ WebGL.toHtml ([ width (Tuple.first viewportSize)
                                  , height (Tuple.second viewportSize)
                                  , style "display" "block"
                                  , style "height" "100vh"
                                  , style "width" "100vw"
                                  , id "webgl-canvas" ] ++ attr)
                       inner ])
        


module HUD.Types exposing
    ( Canvas
    , CanvasDimensions
    , RenderData
    )


type alias Canvas =
    { canvasDim : CanvasDimensions
    , renderData : RenderData
    , overviewToggle : Bool
    }


type alias CanvasDimensions =
    { width : Int
    , height : Int
    }


type alias RenderData =
    { elapsed : Float
    , previousElapsed : Float
    }

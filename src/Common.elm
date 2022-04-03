module Common exposing (Model)

type alias Model =
  { location : { x : Float, y: Float, z: Float }
  , rotation : Float
  , elapsed : Float
  , fireStrength : Float
  , pointerOffset : { x: Int, y: Int }
  , canvasDimensions : { width: Int, height: Int }
  }


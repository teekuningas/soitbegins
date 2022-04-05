module Common exposing (Model, viewportSize, meshPositionMap, MeshList, Vertex, Uniforms)


viewportSize : (Int, Int)
viewportSize = (800, 800)


type alias Model =
  { location : { x : Float, y: Float, z: Float }
  , rotation : Float
  , elapsed : Float
  , fireStrength : Float
  , pointerOffset : { x: Int, y: Int }
  , canvasDimensions : { width: Int, height: Int }
  }


meshPositionMap : (Vec3 -> Vec3) -> MeshList -> MeshList
meshPositionMap fun mesh =
  case mesh of
    [] ->
      []
    (v1, v2, v3) :: xs ->
      [ ( { v1 | position = fun v1.position }
        , { v2 | position = fun v2.position }
        , { v3 | position = fun v3.position } ) ] ++ (meshPositionMap fun xs)


type alias Uniforms =
  { rotation : Mat4
  , location : Mat4
  , perspective : Mat4
  , camera : Mat4
  , scale : Mat4
  , shade : Float
  }


type alias Vertex =
  { color : Vec3
  , position : Vec3
  }


type alias MeshList = List (Vertex, Vertex, Vertex)


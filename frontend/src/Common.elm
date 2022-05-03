module Common exposing
    ( Camera
    , CanvasDimensions
    , ConnectionData
    , ConnectionState(..)
    , Controller
    , Data
    , DragState(..)
    , Earth
    , GameData
    , GameState(..)
    , Hero
    , MeshList
    , Model
    , RenderData
    , Uniforms
    , Vertex
    , fragmentShader
    , meshPositionMap
    , vertexShader
    , viewportSize
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import WebGL exposing (Mesh, Shader)


viewportSize : ( Int, Int )
viewportSize =
    ( 800, 800 )


type alias Model =
    { gameState : GameState
    , connectionState : ConnectionState
    , data : Data
    }


type alias Data =
    { earthMesh : Maybe (Mesh Vertex)
    }


type GameState
    = MainMenu
    | FlightMode GameData
    | InitializationFailed


type alias GameData =
    { earth : Maybe Earth
    , camera : Camera
    , controller : Controller
    , hero : Hero
    , renderData : Maybe RenderData
    , canvasDimensions : Maybe CanvasDimensions
    }


type alias CanvasDimensions =
    { width : Int
    , height : Int
    }


type alias Hero =
    { altitude : Float
    , latitude : Float
    , longitude : Float
    , rotationTheta : Float
    , power : Float
    }


type alias Earth =
    { locationX : Float
    , locationY : Float
    , locationZ : Float
    , rotationTheta : Float
    }


type alias Camera =
    { azimoth : Float
    , elevation : Float
    }


type alias RenderData =
    { elapsed : Float
    , previousElapsed : Maybe Float
    }


type DragState
    = Drag
    | NoDrag


type alias Controller =
    { dragState : DragState
    , pointerOffset : { x : Int, y : Int }
    , previousOffset : { x : Int, y : Int }
    , downButtonDown : Bool
    , upButtonDown : Bool
    }


type ConnectionState
    = Connected ConnectionData
    | Disconnected


type alias ConnectionData =
    { earth :
        Maybe
            { msgEarth : Earth
            , previousMsgEarth : Earth
            }
    , elapsed :
        Maybe
            { msgElapsed : Float
            , previousMsgElapsed : Maybe Float
            }
    }



-- WebGL-related types


type alias Uniforms =
    { preScale : Mat4
    , preRotation : Mat4
    , preTranslation : Mat4
    , scale : Mat4
    , rotation : Mat4
    , translation : Mat4
    , postScale : Mat4
    , postRotation : Mat4
    , postTranslation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


type alias MeshList =
    List ( Vertex, Vertex, Vertex )



-- Vertex shader


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
     attribute vec3 position;
     attribute vec3 color;
     uniform mat4 perspective;
     uniform mat4 camera;
     uniform mat4 preScale;
     uniform mat4 preRotation;
     uniform mat4 preTranslation;
     uniform mat4 scale;
     uniform mat4 rotation;
     uniform mat4 translation;
     uniform mat4 postScale;
     uniform mat4 postRotation;
     uniform mat4 postTranslation;
     varying vec3 vcolor;
     void main () {
       gl_Position = (perspective * camera * 
                      postTranslation * postRotation * postScale * 
                      translation * rotation * scale *
                      preTranslation * preRotation * preScale * vec4(position, 1.0));
       vcolor = color;
     }
  |]



-- Fragment shader


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
    precision mediump float;
    uniform float shade;
    varying vec3 vcolor;
    void main () {
      gl_FragColor = shade * vec4(vcolor, 1.0);
    }
  |]



-- A utility map for Vertex positions


meshPositionMap : (Vec3 -> Vec3) -> MeshList -> MeshList
meshPositionMap fun mesh =
    case mesh of
        [] ->
            []

        ( v1, v2, v3 ) :: xs ->
            ( { v1 | position = fun v1.position }
            , { v2 | position = fun v2.position }
            , { v3 | position = fun v3.position }
            )
                :: meshPositionMap fun xs

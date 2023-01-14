module World.Types exposing
    ( Camera
    , Controller
    , Data
    , DragState(..)
    , Earth
    , Hero
    , MeshList
    , Uniforms
    , Vertex
    , World
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)


type alias Data =
    { earthMesh : Mesh Vertex
    , serverUpdateInterval : Int
    }


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


type alias World =
    { earth : Earth
    , camera : Camera
    , hero : Hero
    , controller : Controller
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


type alias Hero =
    { altitude : Float
    , latitude : Float
    , longitude : Float
    , speed : Float
    , direction : Vec3
    , rotationTheta : Float
    , power : Float
    , envColor : Vec3
    }


type alias Earth =
    { rotationAroundSun : Float
    , rotationAroundAxis : Float
    }


type alias Camera =
    { azimoth : Float
    , elevation : Float
    }

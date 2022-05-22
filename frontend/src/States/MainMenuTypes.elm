module States.MainMenuTypes exposing (MenuData)

import World.Types exposing (Vertex, MeshList)
import WebGL exposing (Mesh)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias MenuData =
    { earthMesh : Mesh Vertex
    , canvasDimensions : CanvasDimensions
    , user : User
    }


type alias User =
    { name : String
    }


type alias CanvasDimensions =
    { width : Int
    , height : Int
    }


module States.GatherInfoTypes exposing (GatherInfoData)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)
import World.Types exposing (MeshList, Vertex)


type alias GatherInfoData =
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

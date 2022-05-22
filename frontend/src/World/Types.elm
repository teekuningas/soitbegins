module World.Types exposing (Vertex, MeshList)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias Vertex = 
    { color : Vec3
    , position : Vec3
    }

type alias MeshList =
    List ( Vertex, Vertex, Vertex )

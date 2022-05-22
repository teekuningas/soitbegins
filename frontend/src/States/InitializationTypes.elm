module States.InitializationTypes exposing (InitData)

import World.Types exposing (Vertex, MeshList)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias InitData =
    { canvasDimensions : CanvasDimensions
    }


type alias CanvasDimensions =
    { width : Int
    , height : Int
    }


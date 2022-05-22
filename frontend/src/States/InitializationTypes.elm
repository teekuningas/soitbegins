module States.InitializationTypes exposing (InitData)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import World.Types exposing (MeshList, Vertex)


type alias InitData =
    { canvasDimensions : CanvasDimensions
    }


type alias CanvasDimensions =
    { width : Int
    , height : Int
    }

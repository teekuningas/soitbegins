module States.InGameLoaderTypes exposing (GameLoaderData)

import World.Types exposing (Vertex, MeshList)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)



type alias GameLoaderData =
    { earth : Maybe Earth
    , renderData : Maybe PreparingRenderData
    , connectionData : Maybe PreparingConnectionData
    , canvasDimensions : CanvasDimensions
    , earthMesh : Mesh Vertex
    , user : User
    , hero : Maybe Hero
    }


type alias User =
    { name : String
    }


type alias CanvasDimensions =
    { width : Int
    , height : Int
    }


type alias Hero =
    { altitude : Float
    , latitude : Float
    , longitude : Float
    , latSpeed : Float
    , lonSpeed : Float
    , rotationTheta : Float
    , power : Float
    , envColor : Vec3
    }


type alias Earth =
    { rotationAroundSun : Float
    , rotationAroundAxis : Float
    }


type alias PreparingRenderData =
    { elapsed : Float
    , previousElapsed : Maybe Float
    }


type alias PreparingConnectionData =
    { earth :
        Maybe
            { msgEarth : Earth
            , previousMsgEarth : Maybe Earth
            }
    , elapsed :
        Maybe
            { msgElapsed : Float
            , previousMsgElapsed : Maybe Float
            }
    }


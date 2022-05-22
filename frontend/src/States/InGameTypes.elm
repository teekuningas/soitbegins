module States.InGameTypes exposing (GameData, Earth, RenderData)

import World.Types exposing (Vertex, MeshList)
import WebGL exposing (Mesh)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import HUD.Controller


type alias GameData =
    { earth : Earth
    , camera : Camera
    , controller : HUD.Controller.Controller
    , hero : Hero
    , renderData : RenderData
    , canvasDimensions : CanvasDimensions
    , connectionData : ConnectionData
    , earthMesh : Mesh Vertex
    , refreshed : Bool
    , overviewToggle : Bool
    , user : User
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


type alias Camera =
    { azimoth : Float
    , elevation : Float
    }


type alias RenderData =
    { elapsed : Float
    , previousElapsed : Float
    }


type alias ConnectionData =
    { earth :
        { msgEarth : Earth
        , previousMsgEarth : Earth
        }
    , elapsed :
        { msgElapsed : Float
        , previousMsgElapsed : Float
        }
    }


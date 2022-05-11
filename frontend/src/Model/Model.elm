module Model.Model exposing
    ( Camera
    , CanvasDimensions
    , Controller
    , DragState(..)
    , Earth
    , GameData
    , GameLoaderData
    , Hero
    , InitData
    , MenuData
    , MeshList
    , Model(..)
    , RenderData
    )

import Math.Vector3 as Vec3 exposing (Vec3)
import WebGL exposing (Mesh)


type Model
    = Initialization InitData
    | MainMenu MenuData
    | InGameLoader GameLoaderData
    | InGame GameData
    | Termination String


type alias InitData =
    { canvasDimensions : CanvasDimensions
    }


type alias GameLoaderData =
    { earth : Maybe Earth
    , renderData : Maybe PreparingRenderData
    , connectionData : Maybe PreparingConnectionData
    , canvasDimensions : CanvasDimensions
    , earthMesh : Mesh Vertex
    }


type alias GameData =
    { earth : Earth
    , camera : Camera
    , controller : Controller
    , hero : Hero
    , renderData : RenderData
    , canvasDimensions : CanvasDimensions
    , connectionData : ConnectionData
    , earthMesh : Mesh Vertex
    , refreshed : Bool
    }


type alias MenuData =
    { earthMesh : Mesh Vertex
    , canvasDimensions : CanvasDimensions
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


type alias PreparingRenderData =
    { elapsed : Float
    , previousElapsed : Maybe Float
    }


type alias RenderData =
    { elapsed : Float
    , previousElapsed : Float
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


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


type alias MeshList =
    List ( Vertex, Vertex, Vertex )

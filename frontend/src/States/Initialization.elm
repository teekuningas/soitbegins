module States.Initialization exposing (Initializing, Msg(..), init, subscriptions, update, view)

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onResize)
import Communication.Flags exposing (FlagsValue)
import Communication.ObjReceiver as ObjReceiver
import HUD.Page
    exposing
        ( embedInCanvas
        , viewportSize
        )
import HUD.Types exposing (Canvas)
import Html
    exposing
        ( Html
        , div
        , p
        , text
        )
import Html.Attributes exposing (class)
import Http
import Json.Decode
import Json.Encode
import Math.Vector3 exposing (Vec3, vec3)
import Platform.Cmd
import Platform.Sub
import Process
import Task
import Time
import WebGL exposing (Mesh)
import World.Types exposing (MeshList, Vertex)


type alias Initializing =
    { num : Int
    , serverUpdateInterval : Int
    }


type Msg
    = ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ObjReceived Json.Decode.Value
    | TransitionToGatherInfoMsg { earthMesh : Mesh Vertex, serverUpdateInterval : Int }
    | TransitionToTerminationMsg String
    | Tick Time.Posix


type alias InitData =
    { initializing : Initializing
    }


init : FlagsValue -> ( InitData, Cmd Msg )
init flags =
    let
        initializing =
            { serverUpdateInterval = flags.serverUpdateInterval
            , num = 0
            }
    in
    ( { initializing = initializing }
    , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
    )


view : Initializing -> Html Msg
view initializing =
    embedInCanvas
        []
        [ div
            [ class "initialization-container" ]
            [ p [] [ text (String.append "Loading assets.. " (String.fromInt initializing.num)) ] ]
        ]
        []
        []


type alias UpdateData =
    { initializing : Initializing
    }


update : Msg -> UpdateData -> ( UpdateData, Cmd Msg )
update msg values =
    case msg of
        -- Refresh canvas
        ResizeMsg ->
            ( values, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

        ObjReceived value ->
            let
                meshResult =
                    Json.Decode.decodeValue meshDecoder value
            in
            case meshResult of
                Ok mesh ->
                    let
                        transitionData =
                            { earthMesh = WebGL.triangles mesh
                            , serverUpdateInterval = values.initializing.serverUpdateInterval
                            }
                    in
                    ( values
                    , Task.perform (always (TransitionToGatherInfoMsg transitionData)) (Task.succeed ())
                    )

                Err errMessage ->
                    ( values
                    , Task.perform (always (TransitionToTerminationMsg "Could not download the assets")) (Task.succeed ())
                    )

        Tick newTime ->
            let
                initializing =
                    values.initializing

                newInitializing =
                    { initializing | num = initializing.num + 1 }
            in
            ( { values | initializing = newInitializing }
            , Cmd.none
            )

        _ ->
            ( values, Cmd.none )


subscriptions : Sub Msg
subscriptions =
    Platform.Sub.batch
        [ onResize (\width height -> ResizeMsg)
        , ObjReceiver.objReceiver ObjReceived
        , Time.every 1000 Tick
        ]



-- Helpers


vec3Decoder : Json.Decode.Decoder Vec3
vec3Decoder =
    let
        toVec3 =
            \x -> \y -> \z -> vec3 x y z
    in
    Json.Decode.map3 toVec3
        (Json.Decode.index 0 Json.Decode.float)
        (Json.Decode.index 1 Json.Decode.float)
        (Json.Decode.index 2 Json.Decode.float)


vertexDecoder : Json.Decode.Decoder Vertex
vertexDecoder =
    Json.Decode.map2 Vertex
        (Json.Decode.field "color" vec3Decoder)
        (Json.Decode.field "position" vec3Decoder)


faceDecoder : Json.Decode.Decoder ( Vertex, Vertex, Vertex )
faceDecoder =
    let
        toTriple =
            \x -> \y -> \z -> ( x, y, z )
    in
    Json.Decode.map3 toTriple
        (Json.Decode.index 0 vertexDecoder)
        (Json.Decode.index 1 vertexDecoder)
        (Json.Decode.index 2 vertexDecoder)


meshDecoder : Json.Decode.Decoder MeshList
meshDecoder =
    Json.Decode.list faceDecoder

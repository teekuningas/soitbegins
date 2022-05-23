module States.Initialization exposing (Msg(..), init, subscriptions, update, view)

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onResize)
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
import Length exposing (Meters, meters)
import Obj.Decode exposing (expectObj)
import Platform.Cmd
import Platform.Sub
import Task
import WebGL exposing (Mesh)
import World.ObjLoader as ObjLoader
import World.Types exposing (Vertex)
import Communication.ObjReceiver as ObjReceiver


type Msg
    = ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ObjReceived (Mesh Vertex)
    | EarthMeshLoaded (Mesh Vertex)
    | EarthMeshLoadFailed String
    | TransitionToGatherInfoMsg { earthMesh : Mesh Vertex }
    | TransitionToTerminationMsg String


init : ( (), Cmd Msg )
init =
    ( ()
    , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
    )


view : Html Msg
view =
    embedInCanvas
        []
        [ div
            [ class "initialization-container" ]
            [ p [] [ text "Loading assets.." ] ]
        ]
        []
        []


update : Msg -> ( (), Cmd Msg )
update msg =
    case msg of
        -- Refresh canvas
        ResizeMsg ->
            ( (), Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

        ViewportMsg returnValue ->
            ( (), Cmd.none )

        EarthMeshLoaded mesh ->
            let
                transitionData =
                    { earthMesh = mesh
                    }
            in
            ( ()
            , Task.perform (always (TransitionToGatherInfoMsg transitionData)) (Task.succeed ())
            )

        EarthMeshLoadFailed err ->
            ( ()
            , Task.perform (always (TransitionToTerminationMsg "Could not download assets")) (Task.succeed ())
            )

        _ ->
            ( (), Cmd.none )


subscriptions : Sub Msg
subscriptions =
    Platform.Sub.batch
        [ onResize (\width height -> ResizeMsg)
        , ObjReceiver.objReceiver meshFromString
        ]


-- Helpers


meshFromString : String -> Msg
meshFromString value =
    let
        objData = (Obj.Decode.decodeString
                    meters
                    ObjLoader.earthMeshDecoder
                    value
                  )
    in
        case objData of
            Ok data ->
                EarthMeshLoaded data
            Err errorMessage ->
                EarthMeshLoadFailed errorMessage
                

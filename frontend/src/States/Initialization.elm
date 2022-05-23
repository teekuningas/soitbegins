module States.Initialization exposing (Msg(..), init, subscriptions, update, view)

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onResize)
import Process
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
import Time


type Msg
    = ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ObjReceived String
    | EarthMeshLoaded (Maybe (Mesh Vertex))
    | TransitionToGatherInfoMsg { earthMesh : Mesh Vertex }
    | TransitionToTerminationMsg String
    | Tick Time.Posix


init : Int -> ( Int , Cmd Msg )
init num =
    ( num
    , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
    )


view : Int -> Html Msg
view num =
    embedInCanvas
        []
        [ div
            [ class "initialization-container" ]
            [ p [] [ text (String.append "Loading assets.. " (String.fromInt num)) ] ]
        ]
        []
        []


update : Msg -> Int -> ( Int, Cmd Msg )
update msg num =
    case msg of

        -- Refresh canvas
        ResizeMsg ->
            ( num, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

        ViewportMsg returnValue ->
            ( num, Cmd.none )

        EarthMeshLoaded maybeMesh ->
            case maybeMesh of
                Just mesh ->
                    let
                        transitionData =
                            { earthMesh = mesh
                            }
                    in
                    ( num
                    , Task.perform (always (TransitionToGatherInfoMsg transitionData)) (Task.succeed ())
                    )
                Nothing ->
                    (num
                    , Task.perform (always (TransitionToTerminationMsg "Could not download assets")) (Task.succeed ())
                    )

        Tick newTime ->
            ( num + 1
            , Cmd.none
            )

        ObjReceived value ->
            ( num
            , Task.perform EarthMeshLoaded (Task.succeed (mesher value))
            )

        _ ->
            ( num, Cmd.none )

    

subscriptions : Sub Msg
subscriptions =
    Platform.Sub.batch
        [ onResize (\width height -> ResizeMsg)
        , ObjReceiver.objReceiver ObjReceived
        , Time.every 1000 Tick
        ]


-- Helpers


mesher : String -> Maybe (Mesh Vertex)
mesher str = 
    let
        res = (Obj.Decode.decodeString
                meters
                ObjLoader.earthMeshDecoder
                str
               )
    in
        case res of
            Ok data ->
                Just data
            Err errorMessage ->
                Nothing


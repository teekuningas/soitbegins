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
import Length exposing (Meters, meters)
import Obj.Decode exposing (expectObj)
import Platform.Cmd
import Platform.Sub
import Process
import Task
import Time
import WebGL exposing (Mesh)
import World.ObjLoader as ObjLoader
import World.Types exposing (Vertex)


type alias Initializing =
    { num : Int
    , serverUpdateInterval : Int
    }


type Msg
    = ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ObjReceived String
    | EarthMeshLoaded (Maybe (Mesh Vertex))
    | TransitionToGatherInfoMsg { earthMesh : Mesh Vertex, serverUpdateInterval : Int }
    | TransitionToTerminationMsg String
    | Tick Time.Posix


init : FlagsValue -> ( { initializing : Initializing }, Cmd Msg )
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


update : Msg -> { initializing : Initializing } -> ( { initializing : Initializing }, Cmd Msg )
update msg values =
    case msg of
        -- Refresh canvas
        ResizeMsg ->
            ( values, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

        EarthMeshLoaded maybeMesh ->
            case maybeMesh of
                Just mesh ->
                    let
                        transitionData =
                            { earthMesh = mesh
                            , serverUpdateInterval = values.initializing.serverUpdateInterval
                            }
                    in
                    ( values
                    , Task.perform (always (TransitionToGatherInfoMsg transitionData)) (Task.succeed ())
                    )

                Nothing ->
                    ( values
                    , Task.perform (always (TransitionToTerminationMsg "Could not download assets")) (Task.succeed ())
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

        ObjReceived value ->
            ( values
            , Task.perform EarthMeshLoaded (Task.succeed (mesher value))
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


mesher : String -> Maybe (Mesh Vertex)
mesher str =
    let
        res =
            Obj.Decode.decodeString
                meters
                ObjLoader.earthMeshDecoder
                str
    in
    case res of
        Ok data ->
            Just data

        Err errorMessage ->
            Nothing

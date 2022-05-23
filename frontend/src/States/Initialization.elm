module States.Initialization exposing (Msg(..), init, subscriptions, update, view)

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onResize)
import Communication.Flags exposing (FlagsValue)
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


type Msg
    = ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | EarthMeshLoaded (Result Http.Error (Mesh Vertex))
    | TransitionToGatherInfoMsg { earthMesh : Mesh Vertex }
    | TransitionToTerminationMsg String


init : FlagsValue -> ( (), Cmd Msg )
init flags =
    let
        modelEarthUrl =
            flags.modelEarth

        cmd =
            Platform.Cmd.batch
                [ Http.get
                    { url = modelEarthUrl
                    , expect =
                        expectObj
                            EarthMeshLoaded
                            meters
                            ObjLoader.earthMeshDecoder
                    }
                , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
                ]
    in
    ( ()
    , cmd
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

        EarthMeshLoaded result ->
            case result of
                Ok mesh ->
                    let
                        transitionData =
                            { earthMesh = mesh
                            }
                    in
                    ( ()
                    , Task.perform (always (TransitionToGatherInfoMsg transitionData)) (Task.succeed ())
                    )

                Err _ ->
                    ( ()
                    , Task.perform (always (TransitionToTerminationMsg "Could not download assets")) (Task.succeed ())
                    )

        _ ->
            ( (), Cmd.none )


subscriptions : Sub Msg
subscriptions =
    Platform.Sub.batch
        [ onResize (\width height -> ResizeMsg)
        ]

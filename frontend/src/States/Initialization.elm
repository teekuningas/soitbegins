module States.Initialization exposing (Msg(..), init, subscriptions, update, view)

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onResize)
import Communication.Flags exposing (FlagsValue)
import HUD.Page
    exposing
        ( embedInCanvas
        , viewportSize
        )
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
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Obj.Decode exposing (expectObj)
import Platform.Cmd
import Platform.Sub
import States.GatherInfoTypes exposing (GatherInfoData)
import States.InitializationTypes exposing (InitData)
import Task
import WebGL exposing (Mesh)
import World.ObjLoader as ObjLoader
import World.Types exposing (MeshList, Vertex)


type Msg
    = ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | EarthMeshLoaded (Result Http.Error (Mesh Vertex))
    | TransitionToGatherInfoMsg GatherInfoData
    | TransitionToTerminationMsg String


init : FlagsValue -> ( InitData, Cmd Msg )
init flags =
    let
        initData =
            { canvasDimensions =
                { width = Tuple.first viewportSize
                , height = Tuple.second viewportSize
                }
            }

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
    ( initData
    , cmd
    )


view : InitData -> Html Msg
view data =
    embedInCanvas
        []
        [ div
            [ class "initialization-container" ]
            [ p [] [ text "Loading assets.." ] ]
        ]
        []
        []


update : Msg -> InitData -> ( InitData, Cmd Msg )
update msg initData =
    case msg of
        ResizeMsg ->
            ( initData, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

        ViewportMsg returnValue ->
            let
                newCanvasDimensions =
                    returnValue
                        |> Result.map .viewport
                        |> Result.map
                            (\v ->
                                { width = round v.width
                                , height = round v.height
                                }
                            )
                        |> Result.withDefault initData.canvasDimensions

                newInitData =
                    { initData | canvasDimensions = newCanvasDimensions }
            in
            ( newInitData
            , Cmd.none
            )

        EarthMeshLoaded result ->
            case result of
                Ok mesh ->
                    let
                        newGatherInfoData =
                            { earthMesh = mesh
                            , canvasDimensions = initData.canvasDimensions
                            , user =
                                { name = "" }
                            }
                    in
                    ( initData
                    , Task.perform (always (TransitionToGatherInfoMsg newGatherInfoData)) (Task.succeed ())
                    )

                Err _ ->
                    ( initData
                    , Task.perform (always (TransitionToTerminationMsg "Could not download assets")) (Task.succeed ())
                    )

        _ ->
            ( initData, Cmd.none )


subscriptions : InitData -> Sub Msg
subscriptions initData =
    Platform.Sub.batch
        [ onResize (\width height -> ResizeMsg)
        ]

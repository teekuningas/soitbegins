module States.InGame exposing (Msg, subscriptions, update, view)

import Browser
import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onAnimationFrame, onResize)
import Common
    exposing
        ( DragState(..)
        , GameData
        , Model(..)
        , Vertex
        , fragmentShader
        , vertexShader
        , viewportSize
        )
import Controller
    exposing
        ( controllerMeshDown
        , controllerMeshUp
        , controllerUnif
        , handleDown
        , handleMove
        , handleUp
        )
import Flags
import Html exposing (Html, button, div, p, span, text)
import Html.Attributes exposing (class, height, id, style, width)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Http
import Json.Decode
import Length exposing (Meters, meters)
import List
import Math.Vector3 as Vec3 exposing (vec3)
import Obj.Decode exposing (expectObj)
import ObjLoader
import Page exposing (embedInCanvas)
import Platform.Cmd
import Platform.Sub
import Receiver
import Task
import Time
import Update exposing (updateGameData)
import WebGL exposing (Mesh)
import Widgets exposing (fpsOverlay)
import World
    exposing
        ( axisMesh
        , axisUnif
        , earthUnif
        , fireMesh
        , fireUnif
        , heroMesh
        , heroUnif
        , sunMesh
        , sunUnif
        )


type Msg
    = TimeElapsed Time.Posix
    | ResizeMsg
    | PointerEventMsg PointerEvent
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | RecvServerMsg Receiver.RecvServerValue
    | RecvServerMsgError String
    | UpdateTimeMsg Time.Posix


type PointerEvent
    = MouseUp Mouse.Event
    | MouseDown Mouse.Event
    | MouseMove Mouse.Event
    | TouchMove Touch.Event
    | TouchDown Touch.Event
    | TouchUp Touch.Event


subscriptions : GameData -> Sub Msg
subscriptions gameData =
    Platform.Sub.batch
        [ onAnimationFrame (\x -> TimeElapsed x)
        , onResize (\width height -> ResizeMsg)
        , Receiver.messageReceiver recvServerJson
        ]


view : GameData -> Html Msg
view gameData =
    let
        earth =
            gameData.earth

        renderData =
            gameData.renderData

        canvasDimensions =
            gameData.canvasDimensions

        camera =
            gameData.camera

        hero =
            gameData.hero

        earthMesh =
            gameData.earthMesh
    in
    embedInCanvas
        [ fpsOverlay renderData
        ]
        [ Touch.onEnd (PointerEventMsg << TouchUp)
        , Touch.onStart (PointerEventMsg << TouchDown)
        , Touch.onMove (PointerEventMsg << TouchMove)
        , Mouse.onUp (PointerEventMsg << MouseUp)
        , Mouse.onDown (PointerEventMsg << MouseDown)
        , Mouse.onMove (PointerEventMsg << MouseMove)
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            heroMesh
            (heroUnif canvasDimensions earth hero camera)
        , WebGL.entity
            vertexShader
            fragmentShader
            fireMesh
            (fireUnif canvasDimensions earth hero camera)
        , WebGL.entity
            vertexShader
            fragmentShader
            earthMesh
            (earthUnif canvasDimensions earth hero camera)
        , WebGL.entity
            vertexShader
            fragmentShader
            axisMesh
            (axisUnif canvasDimensions earth hero camera)
        , WebGL.entity
            vertexShader
            fragmentShader
            sunMesh
            (sunUnif canvasDimensions earth hero camera)
        , WebGL.entity
            vertexShader
            fragmentShader
            controllerMeshUp
            (controllerUnif canvasDimensions
                (if gameData.controller.upButtonDown then
                    1.0

                 else
                    0.5
                )
            )
        , WebGL.entity
            vertexShader
            fragmentShader
            controllerMeshDown
            (controllerUnif canvasDimensions
                (if gameData.controller.downButtonDown then
                    1.0

                 else
                    0.5
                )
            )
        ]


update : Msg -> GameData -> ( Model, Cmd Msg )
update msg gameData =
    case msg of
        RecvServerMsgError message ->
            let
                newGameLoaderData =
                    { earth = Nothing
                    , renderData = Nothing
                    , connectionData = Nothing
                    , earthMesh = gameData.earthMesh
                    , canvasDimensions = gameData.canvasDimensions
                    }
            in
            ( InGameLoader newGameLoaderData, Cmd.none )

        RecvServerMsg message ->
            let
                msgEarth =
                    { locationX = message.earth.locationX
                    , locationY = message.earth.locationY
                    , locationZ = message.earth.locationZ
                    , rotationTheta = message.earth.rotationTheta
                    }

                newEarth =
                    { msgEarth = msgEarth
                    , previousMsgEarth =
                        gameData.connectionData.earth.msgEarth
                    }

                connectionData =
                    gameData.connectionData

                newConnectionData =
                    { connectionData | earth = newEarth }

                newGameData =
                    { gameData | connectionData = newConnectionData }
            in
            ( InGame newGameData
            , Task.perform UpdateTimeMsg Time.now
            )

        UpdateTimeMsg dt ->
            let
                msgElapsed =
                    toFloat (Time.posixToMillis dt)

                newElapsedData =
                    { msgElapsed = msgElapsed
                    , previousMsgElapsed =
                        gameData.connectionData.elapsed.msgElapsed
                    }

                connectionData =
                    gameData.connectionData

                newConnectionData =
                    { connectionData | elapsed = newElapsedData }

                newGameData =
                    { gameData | connectionData = newConnectionData }
            in
            ( InGame newGameData
            , Cmd.none
            )

        TimeElapsed dt ->
            let
                elapsed =
                    toFloat (Time.posixToMillis dt)

                previousElapsed =
                    gameData.renderData.elapsed

                newRenderData =
                    { elapsed = elapsed
                    , previousElapsed = previousElapsed
                    }

                connectionData =
                    gameData.connectionData

                earthData =
                    connectionData.earth

                elapsedData =
                    connectionData.elapsed

                updatedGameData =
                    updateGameData
                        elapsed
                        previousElapsed
                        elapsedData.msgElapsed
                        elapsedData.previousMsgElapsed
                        earthData.msgEarth
                        earthData.previousMsgEarth
                        gameData

                cmd = 
                    if updatedGameData.refreshed == False 
                    then
                        Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
                    else
                        Cmd.none

                newGameData =
                    { updatedGameData | renderData = newRenderData
                                      , refreshed = True }
            in
            ( InGame newGameData
            , cmd
            )

        PointerEventMsg event ->
            case event of
                MouseUp struct ->
                    let
                        newController =
                            handleUp gameData.controller

                        newGameData =
                            { gameData | controller = newController }
                    in
                    ( InGame newGameData, Cmd.none )

                MouseDown struct ->
                    let
                        newController =
                            handleDown
                                gameData.controller
                                struct.offsetPos
                                gameData.canvasDimensions

                        newGameData =
                            { gameData | controller = newController }
                    in
                    ( InGame newGameData, Cmd.none )

                MouseMove struct ->
                    let
                        ( newController, newCamera ) =
                            handleMove
                                gameData.controller
                                gameData.camera
                                struct.offsetPos

                        newGameData =
                            { gameData
                                | controller = newController
                                , camera = newCamera
                            }
                    in
                    ( InGame newGameData
                    , Cmd.none
                    )

                TouchUp struct ->
                    let
                        controller =
                            gameData.controller

                        newController =
                            { controller
                                | upButtonDown = False
                                , downButtonDown = False
                                , dragState = NoDrag
                            }

                        newGameData =
                            { gameData | controller = newController }
                    in
                    ( InGame newGameData
                    , Cmd.none
                    )

                TouchDown struct ->
                    case List.head struct.touches of
                        Nothing ->
                            ( InGame gameData, Cmd.none )

                        Just x ->
                            let
                                newController =
                                    handleDown
                                        gameData.controller
                                        x.clientPos
                                        gameData.canvasDimensions

                                newGameData =
                                    { gameData | controller = newController }
                            in
                            ( InGame newGameData
                            , Cmd.none
                            )

                TouchMove struct ->
                    case List.head struct.touches of
                        Nothing ->
                            ( InGame gameData, Cmd.none )

                        Just x ->
                            let
                                ( newController, newCamera ) =
                                    handleMove
                                        gameData.controller
                                        gameData.camera
                                        x.clientPos

                                newGameData =
                                    { gameData
                                        | controller = newController
                                        , camera = newCamera
                                    }
                            in
                            ( InGame newGameData
                            , Cmd.none
                            )

        ResizeMsg ->
            ( InGame gameData, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

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
                        |> Result.withDefault gameData.canvasDimensions

                newGameData =
                    { gameData | canvasDimensions = newCanvasDimensions }
            in
            ( InGame newGameData
            , Cmd.none
            )



-- Some helpers.


recvServerJson : String -> Msg
recvServerJson value =
    case Receiver.decodeJson value of
        Ok result ->
            RecvServerMsg result

        Err errorMessage ->
            RecvServerMsgError "Error while communicating with the server"

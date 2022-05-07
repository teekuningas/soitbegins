module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewportOf)
import Browser.Events exposing (onAnimationFrame, onResize)
import Common
    exposing
        ( DragState(..)
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


type PointerEvent
    = MouseUp Mouse.Event
    | MouseDown Mouse.Event
    | MouseMove Mouse.Event
    | TouchMove Touch.Event
    | TouchDown Touch.Event
    | TouchUp Touch.Event


type Msg
    = TimeElapsed Time.Posix
    | ResizeMsg
    | PointerEventMsg PointerEvent
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | RecvServerMsg Receiver.RecvServerValue
    | RecvServerMsgError String
    | UpdateTimeMsg Time.Posix
    | StartGameMsg
    | EarthMeshLoaded (Result Http.Error (Mesh Vertex))



-- The model initialization


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flagsMsg =
    let
        flags =
            Json.Decode.decodeValue Flags.flagsDecoder flagsMsg
    in
    case flags of
        Err _ ->
            ( Termination "Could not read environment variables"
            , Cmd.none
            )

        Ok value ->
            let
                initData =
                    { canvasDimensions =
                        { width = Tuple.first viewportSize
                        , height = Tuple.second viewportSize
                        }
                    }

                modelEarthUrl =
                    value.modelEarth

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
            ( Initialization initData
            , cmd
            )



-- The view function


view : Model -> Html Msg
view model =
    case model of
        Termination msg ->
            div [] [ text msg ]

        Initialization _ ->
            embedInCanvas
                [ div
                    [ class "initialization-container" ]
                    [ p [] [ text "Loading assets.." ]
                    ]
                ]
                []
                []

        MainMenu _ ->
            embedInCanvas
                [ div
                    [ class "main-menu-container" ]
                    [ p [] [ text "So it begins (the grand hot air balloon adventure)" ]
                    , button [ onClick StartGameMsg ] [ text "Start here" ]
                    ]
                ]
                []
                []

        InGameLoader gameLoaderData ->
            embedInCanvas
                [ div
                    [ class "game-loader-container" ]
                    [ p [] [ text "Loading game.." ]
                    , p [] [ text (Debug.toString gameLoaderData.connectionData) ]
                    , p [] [ text (Debug.toString gameLoaderData.earth) ]
                    , p [] [ text (Debug.toString gameLoaderData.renderData) ]
                    ]
                ]
                []
                []

        InGame gameData ->
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



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Platform.Sub.batch
        [ onAnimationFrame (\x -> TimeElapsed x)
        , onResize (\width height -> ResizeMsg)
        , Receiver.messageReceiver recvServerJson
        ]



-- Updates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGameMsg ->
            case model of
                MainMenu menuData ->
                    let
                        gameLoaderData =
                            { earthMesh = menuData.earthMesh
                            , renderData = Nothing
                            , connectionData = Nothing
                            , earth = Nothing
                            , canvasDimensions = menuData.canvasDimensions
                            }
                    in
                    ( InGameLoader gameLoaderData
                    , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        RecvServerMsgError message ->
            case model of
                InGameLoader gameLoaderData ->
                    let
                        newGameLoaderData =
                            { gameLoaderData | connectionData = Nothing }
                    in
                    ( InGameLoader gameLoaderData, Cmd.none )

                InGame gameData ->
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

                _ ->
                    ( model, Cmd.none )

        RecvServerMsg message ->
            case model of
                InGameLoader gameLoaderData ->
                    let
                        msgEarth =
                            { locationX = message.earth.locationX
                            , locationY = message.earth.locationY
                            , locationZ = message.earth.locationZ
                            , rotationTheta = message.earth.rotationTheta
                            }
                    in
                    case gameLoaderData.connectionData of
                        Just preparingConnectionData ->
                            let
                                newEarth =
                                    { msgEarth = msgEarth
                                    , previousMsgEarth =
                                        preparingConnectionData.earth
                                            |> Maybe.map .msgEarth
                                            |> Maybe.withDefault msgEarth
                                            |> Just
                                    }

                                newConnectionData =
                                    { preparingConnectionData | earth = Just newEarth }

                                newGameLoaderData =
                                    { gameLoaderData | connectionData = Just newConnectionData }
                            in
                            ( InGameLoader newGameLoaderData
                            , Task.perform UpdateTimeMsg Time.now
                            )

                        Nothing ->
                            let
                                newConnectionData =
                                    { earth =
                                        Just
                                            { msgEarth = msgEarth
                                            , previousMsgEarth = Nothing
                                            }
                                    , elapsed = Nothing
                                    }

                                newGameLoaderData =
                                    { gameLoaderData | connectionData = Just newConnectionData }
                            in
                            ( InGameLoader newGameLoaderData
                            , Task.perform UpdateTimeMsg Time.now
                            )

                InGame gameData ->
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

                _ ->
                    ( model, Cmd.none )

        UpdateTimeMsg dt ->
            case model of
                InGameLoader gameLoaderData ->
                    case gameLoaderData.connectionData of
                        Just preparingConnectionData ->
                            let
                                msgElapsed =
                                    toFloat (Time.posixToMillis dt)

                                newElapsedData =
                                    { msgElapsed = msgElapsed
                                    , previousMsgElapsed =
                                        preparingConnectionData.elapsed
                                            |> Maybe.map .msgElapsed
                                            |> Maybe.withDefault msgElapsed
                                            |> Just
                                    }

                                newConnectionData =
                                    { preparingConnectionData | elapsed = Just newElapsedData }

                                newGameLoaderData =
                                    { gameLoaderData | connectionData = Just newConnectionData }
                            in
                            ( InGameLoader newGameLoaderData
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                InGame gameData ->
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

                _ ->
                    ( model, Cmd.none )

        TimeElapsed dt ->
            case model of
                InGameLoader gameLoaderData ->
                    let
                        initGameData renderData msgElapsed previousMsgElapsed msgEarth previousMsgEarth =
                            let
                                elapsed =
                                    toFloat (Time.posixToMillis dt)

                                newConnectionData =
                                    { earth =
                                        { msgEarth = msgEarth
                                        , previousMsgEarth = previousMsgEarth
                                        }
                                    , elapsed =
                                        { msgElapsed = msgElapsed
                                        , previousMsgElapsed = previousMsgElapsed
                                        }
                                    }
                            in
                            { earth = msgEarth
                            , camera =
                                { azimoth = 0
                                , elevation = 0
                                }
                            , controller =
                                { dragState = NoDrag
                                , pointerOffset = { x = 0, y = 0 }
                                , previousOffset = { x = 0, y = 0 }
                                , upButtonDown = False
                                , downButtonDown = False
                                }
                            , hero =
                                { altitude = 11
                                , latitude = -0.3
                                , longitude = 0.0
                                , rotationTheta = 0
                                , power = 1
                                }
                            , renderData =
                                { elapsed = elapsed
                                , previousElapsed = renderData.elapsed
                                }
                            , canvasDimensions = gameLoaderData.canvasDimensions
                            , connectionData = newConnectionData
                            , earthMesh = gameLoaderData.earthMesh
                            }
                    in
                    case ( gameLoaderData.renderData, gameLoaderData.connectionData ) of
                        ( Just renderData, Just connectionData ) ->
                            case ( connectionData.elapsed, connectionData.earth ) of
                                ( Just elapsedData, Just earthData ) ->
                                    case ( elapsedData.previousMsgElapsed, earthData.previousMsgEarth ) of
                                        ( Just previousMsgElapsed, Just previousMsgEarth ) ->
                                            let
                                                msgElapsed =
                                                    elapsedData.msgElapsed

                                                msgEarth =
                                                    earthData.msgEarth

                                                newGameData =
                                                    initGameData
                                                        renderData
                                                        msgElapsed
                                                        previousMsgElapsed
                                                        msgEarth
                                                        previousMsgEarth
                                            in
                                            ( InGame newGameData
                                            , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
                                            )

                                        _ ->
                                            let
                                                elapsed =
                                                    toFloat (Time.posixToMillis dt)

                                                newRenderData =
                                                    { elapsed = elapsed
                                                    , previousElapsed = Just renderData.elapsed
                                                    }

                                                newGameLoaderData =
                                                    { gameLoaderData | renderData = Just newRenderData }
                                            in
                                            ( InGameLoader newGameLoaderData
                                            , Cmd.none
                                            )

                                _ ->
                                    let
                                        elapsed =
                                            toFloat (Time.posixToMillis dt)

                                        newRenderData =
                                            { elapsed = elapsed
                                            , previousElapsed = Just renderData.elapsed
                                            }

                                        newGameLoaderData =
                                            { gameLoaderData | renderData = Just newRenderData }
                                    in
                                    ( InGameLoader newGameLoaderData
                                    , Cmd.none
                                    )

                        ( Just renderData, _ ) ->
                            let
                                elapsed =
                                    toFloat (Time.posixToMillis dt)

                                newRenderData =
                                    { elapsed = elapsed
                                    , previousElapsed = Just renderData.elapsed
                                    }

                                newGameLoaderData =
                                    { gameLoaderData | renderData = Just newRenderData }
                            in
                            ( InGameLoader newGameLoaderData
                            , Cmd.none
                            )

                        _ ->
                            let
                                elapsed =
                                    toFloat (Time.posixToMillis dt)

                                newRenderData =
                                    { elapsed = elapsed
                                    , previousElapsed = Nothing
                                    }

                                newGameLoaderData =
                                    { gameLoaderData | renderData = Just newRenderData }
                            in
                            ( InGameLoader newGameLoaderData
                            , Cmd.none
                            )

                InGame gameData ->
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

                        newGameData =
                            { updatedGameData | renderData = newRenderData }
                    in
                    ( InGame newGameData
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PointerEventMsg event ->
            case model of
                InGame gameData ->
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
                                    ( model, Cmd.none )

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
                                    ( model, Cmd.none )

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

                _ ->
                    ( model, Cmd.none )

        ResizeMsg ->
            ( model, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

        -- This Msg is often induced by code also, as it refreshes the screen..
        ViewportMsg returnValue ->
            case model of
                Termination _ ->
                    ( model, Cmd.none )

                Initialization initData ->
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
                    ( Initialization newInitData
                    , Cmd.none
                    )

                MainMenu menuData ->
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
                                |> Result.withDefault menuData.canvasDimensions

                        newMenuData =
                            { menuData | canvasDimensions = newCanvasDimensions }
                    in
                    ( MainMenu newMenuData
                    , Cmd.none
                    )

                InGameLoader gameLoaderData ->
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
                                |> Result.withDefault gameLoaderData.canvasDimensions

                        newGameLoaderData =
                            { gameLoaderData | canvasDimensions = newCanvasDimensions }
                    in
                    ( InGameLoader newGameLoaderData
                    , Cmd.none
                    )

                InGame gameData ->
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

        EarthMeshLoaded result ->
            case model of
                Initialization initData ->
                    case result of
                        Ok mesh ->
                            let
                                newMenuData =
                                    { earthMesh = mesh
                                    , canvasDimensions = initData.canvasDimensions
                                    }
                            in
                            ( MainMenu newMenuData
                            , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
                            )

                        Err _ ->
                            ( Termination "Could not download assets"
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )



-- Here it begins.


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- Some helpers.


recvServerJson : String -> Msg
recvServerJson value =
    case Receiver.decodeJson value of
        Ok result ->
            RecvServerMsg result

        Err errorMessage ->
            RecvServerMsgError "Error while communicating with the server"

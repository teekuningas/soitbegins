module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewportOf)
import Browser.Events exposing (onAnimationFrame, onResize)
import Common
    exposing
        ( CanvasDimensions
        , ConnectionData
        , ConnectionState(..)
        , DragState(..)
        , GameData
        , LoaderData
        , MenuData
        , Model
        , RenderData
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
            Termination "Could not read environment variables"

        Ok value ->
            let 

                initData = 
                    { earthMesh : Nothing
                    }

                modelEarthUrl =
                    value.modelEarth

                cmd =
                    Http.get
                        { url = modelEarthUrl
                        , expect =
                              expectObj
                              EarthMeshLoaded
                              meters
                              ObjLoader.earthMeshDecoder
                        }

            in
            ( Initialization initData
            , cmd )


-- The view function


view : Model -> Html Msg
view model =
    case model
    of
        ( Termination msg) ->
            div [] [ text msg ]

        ( Initialization _ ) ->
            embedInCanvas
                [ div
                    [ class "initialization-container" ]
                    [ p [] [ text "Loading assets.." ]
                    ]
                ]
                []
                []

        ( MainMenu _ ) ->
            embedInCanvas
                [ div
                    [ class "main-menu-container" ]
                    [ p [] [ text "So it begins (the grand hot air balloon adventure)" ]
                    , button [ onClick StartGameMsg ] [ text "Start here" ]
                    ]
                ]
                []
                []

        ( InGameLoader _ ) ->
            embedInCanvas
                [ div
                    [ class "initialization-container" ]
                    [ p [] [ text "Loading game.." ]
                    ]
                ]
                []
                []

        ( InGame gameData ) ->
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

                    gameLoaderData =
                        { earthMesh = menuData.earthMesh
                        , renderData = Nothing
                        , connectionData = Nothing
                        , earth = Nothing
                        }

                _ ->
                    (model, Cmd.none)

--      StartGameMsg ->
--          let
--              gameData =
--                  { earth = Nothing
--                  , camera =
--                      { azimoth = 0
--                      , elevation = 0
--                      }
--                  , controller =
--                      { dragState = NoDrag
--                      , pointerOffset = { x = 0, y = 0 }
--                      , previousOffset = { x = 0, y = 0 }
--                      , upButtonDown = False
--                      , downButtonDown = False
--                      }
--                  , hero =
--                      { altitude = 11
--                      , latitude = -0.3
--                      , longitude = 0.0
--                      , rotationTheta = 0
--                      , power = 1
--                      }
--                  , canvasDimensions = Nothing
--                  , renderData = Nothing
--                  }
--          in
--          ( { model | gameState = FlightMode gameData }
--          , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
--          )

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
                            , renderData = Just gameData.renderData
                            , connectionData = Nothing
                            , earthMesh = gameData.earthMesh }
                    in
                    ( InGameLoader newGameLoaderData, Cmd.none)

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
                    case model.connectionData of
                        Just preparingConnectionData ->
                            let
                                newEarth =
                                    { msgEarth = msgEarth
                                    , previousMsgEarth =
                                        preparingConnectionData.earth
                                            |> Maybe.map .msgEarth
                                            |> Maybe.withDefault msgEarth
                                    }

                                newConnectionData =
                                    { preparingConnectionData | earth = Just newEarth }

                                newGameLoaderData = { gameLoaderData | connectionData = Just newConnectionData }
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

                                newGameLoaderData = { gameLoaderData | connectionData = Just newConnectionData }
                            in
                            ( InGameLoader gameLoaderData
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
                                preparingConnectionData.earth
                            }

                        newConnectionData =
                            { preparingConnectionData | earth = newEarth }

                        newGameData = { gameData | connectionData = newConnectionData }
                        in
                        ( InGame newGameData
                        , Task.perform UpdateTimeMsg Time.now
                        )

                _ ->
                    (model, Cmd.none)

        UpdateTimeMsg dt ->
            case model of
                InGameLoader gameLoaderData ->
                    case model.connectionData of
                        Just preparingconnectionData ->
                            let
                                msgElapsed =
                                    toFloat (Time.posixToMillis dt)

                                newElapsedData =
                                    { msgElapsed = msgElapsed
                                    , previousMsgElapsed =
                                        connectionData.elapsed
                                            |> Maybe.map .msgElapsed
                                            |> Maybe.withDefault msgElapsed
                                            |> Just
                                    }

                                newConnectionData =
                                    { connectionData | elapsed = Just newElapsedData }

                                newGameLoaderData = { gameLoaderData | connectionData = Just newConnectionData }
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
                               gameData.connectionData.elapsed
                           }

                       newConnectionData =
                           { connectionData | elapsed = newElapsedData }

                       newGameData = { gameData | connectionData = newConnectionData }
                    in
                    ( InGame newGameData
                    , Cmd.none
                    )

                _ ->
                    (model, Cmd.none)

        -- Continue here.


        TimeElapsed dt ->
            case model.gameState of
                MainMenu ->
                    ( model, Cmd.none )

                Termination _ ->
                    ( model, Cmd.none )

                FlightMode gameData ->
                    let
                        elapsed =
                            toFloat (Time.posixToMillis dt)
                    in
                    case gameData.renderData of
                        Nothing ->
                            let
                                newRenderData =
                                    { elapsed = elapsed
                                    , previousElapsed = Nothing
                                    }

                                newGameData =
                                    { gameData | renderData = Just newRenderData }
                            in
                            ( { model | gameState = FlightMode newGameData }
                            , Cmd.none
                            )

                        Just renderData ->
                            case renderData.previousElapsed of
                                Nothing ->
                                    let
                                        newRenderData =
                                            { elapsed = elapsed
                                            , previousElapsed = Just renderData.elapsed
                                            }

                                        newGameData =
                                            { gameData | renderData = Just newRenderData }
                                    in
                                    ( { model | gameState = FlightMode newGameData }
                                    , Cmd.none
                                    )

                                Just previousElapsed ->
                                    let
                                        newRenderData =
                                            { elapsed = elapsed
                                            , previousElapsed = Just renderData.elapsed
                                            }
                                    in
                                    case model.connectionData of
                                        Disconnected ->
                                            let
                                                newGameData =
                                                    { gameData | renderData = Just newRenderData }
                                            in
                                            ( { model | gameState = FlightMode newGameData }
                                            , Cmd.none
                                            )

                                        Connected connectionData ->
                                            case ( connectionData.earth, connectionData.elapsed ) of
                                                ( Just earthData, Just elapsedData ) ->
                                                    case elapsedData.previousMsgElapsed of
                                                        Just previousMsgElapsed ->
                                                            let
                                                                updatedGameData =
                                                                    updateGameData
                                                                        elapsed
                                                                        previousElapsed
                                                                        elapsedData.msgElapsed
                                                                        previousMsgElapsed
                                                                        earthData.msgEarth
                                                                        earthData.previousMsgEarth
                                                                        gameData

                                                                newGameData =
                                                                    { updatedGameData | renderData = Just newRenderData }
                                                            in
                                                            ( { model | gameState = FlightMode newGameData }
                                                            , Cmd.none
                                                            )

                                                        Nothing ->
                                                            let
                                                                newGameData =
                                                                    { gameData | renderData = Just newRenderData }
                                                            in
                                                            ( { model | gameState = FlightMode newGameData }
                                                            , Cmd.none
                                                            )

                                                ( _, _ ) ->
                                                    let
                                                        newGameData =
                                                            { gameData | renderData = Just newRenderData }
                                                    in
                                                    ( { model | gameState = FlightMode newGameData }
                                                    , Cmd.none
                                                    )

        -- Here mouse and touch related events are handled
        PointerEventMsg event ->
            case model.gameState of
                MainMenu ->
                    ( model, Cmd.none )

                Termination _ ->
                    ( model, Cmd.none )

                FlightMode gameData ->
                    case event of
                        MouseUp struct ->
                            let
                                newController =
                                    handleUp gameData.controller

                                newGameData =
                                    { gameData | controller = newController }
                            in
                            ( { model | gameState = FlightMode newGameData }, Cmd.none )

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
                            ( { model | gameState = FlightMode newGameData }, Cmd.none )

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
                            ( { model
                                | gameState = FlightMode newGameData
                              }
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
                            ( { model | gameState = FlightMode newGameData }, Cmd.none )

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
                                    ( { model | gameState = FlightMode newGameData }, Cmd.none )

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
                                    ( { model
                                        | gameState = FlightMode newGameData
                                      }
                                    , Cmd.none
                                    )

        ResizeMsg ->
            ( model, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

        -- This Msg is often induced by code also, as it refreshes the screen..
        ViewportMsg returnValue ->
            case returnValue of
                Ok struct ->
                    case model.gameState of
                        FlightMode gameData ->
                            let
                                newCanvasDimensions =
                                    { width = round struct.viewport.width
                                    , height = round struct.viewport.height
                                    }

                                newGameData =
                                    { gameData | canvasDimensions = Just newCanvasDimensions }
                            in
                            ( { model | gameState = FlightMode newGameData }
                            , Cmd.none
                            )

                        MainMenu ->
                            ( model, Cmd.none )

                        Termination _ ->
                            ( model, Cmd.none )

                Err errMsg ->
                    ( model, Cmd.none )

        EarthMeshLoaded result ->
            let
                data =
                    model.data

                newData =
                    { data | earthMesh = Result.toMaybe result }
            in
            ( { model | data = newData }
            , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
            )



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

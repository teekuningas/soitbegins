module States.InGame exposing (Msg(..), init, subscriptions, update, view)

import World.Types exposing (Vertex, MeshList)
import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onAnimationFrame, onResize)
import Communication.Flags
import Communication.Receiver as Receiver
import HUD.Controller
import HUD.Page exposing (embedInCanvas)
import Html exposing (Html, text, span, div)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onMouseDown)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import List
import Platform.Cmd
import Platform.Sub
import Task
import Time
import WebGL exposing (Mesh)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import World.World as World
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
import States.InGameTypes exposing (GameData, Earth, RenderData)
import States.InGameLoaderTypes exposing (GameLoaderData)


type Msg
    = TimeElapsed Time.Posix
    | ResizeMsg
    | PointerEventMsg PointerEvent
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | RecvServerMsg Receiver.RecvServerValue
    | RecvServerMsgError String
    | UpdateTimeMsg Time.Posix
    | OverviewToggleMsg
    | TransitionToInGameLoaderMsg GameLoaderData


type PointerEvent
    = MouseUp Mouse.Event
    | MouseDown Mouse.Event
    | MouseMove Mouse.Event
    | TouchMove Touch.Event
    | TouchDown Touch.Event
    | TouchUp Touch.Event


init : GameData -> ( GameData, Cmd Msg )
init gameData =
    (gameData, Cmd.none)


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

        overviewToggle =
            gameData.overviewToggle

        containerAttrs =
            if overviewToggle then
                [ class "background-black" ]

            else
                []
    in
    embedInCanvas
        containerAttrs
        [ (fpsOverlay renderData)
        , (overviewToggleOverlay gameData.overviewToggle)
        , (debugOverlay gameData)
        ]
        [ Touch.onEnd (PointerEventMsg << TouchUp)
        , Touch.onStart (PointerEventMsg << TouchDown)
        , Touch.onMove (PointerEventMsg << TouchMove)
        , Mouse.onUp (PointerEventMsg << MouseUp)
        , Mouse.onDown (PointerEventMsg << MouseDown)
        , Mouse.onMove (PointerEventMsg << MouseMove)
        ]
        [ WebGL.entity
            World.vertexShader
            World.fragmentShader
            (heroMesh gameData.hero.envColor)
            (heroUnif overviewToggle canvasDimensions earth hero camera)
        , WebGL.entity
            World.vertexShader
            World.fragmentShader
            fireMesh
            (fireUnif overviewToggle canvasDimensions earth hero camera)
        , WebGL.entity
            World.vertexShader
            World.fragmentShader
            earthMesh
            (earthUnif overviewToggle canvasDimensions earth hero camera)
        , WebGL.entity
            World.vertexShader
            World.fragmentShader
            axisMesh
            (axisUnif overviewToggle canvasDimensions earth hero camera)
        , WebGL.entity
            World.vertexShader
            World.fragmentShader
            sunMesh
            (sunUnif overviewToggle canvasDimensions earth hero camera)
        , WebGL.entity
            HUD.Controller.vertexShader
            HUD.Controller.fragmentShader
            HUD.Controller.controllerMeshUp
            (HUD.Controller.controllerUnif canvasDimensions
                (if gameData.controller.upButtonDown then
                    1.0

                 else
                    0.5
                )
            )
        , WebGL.entity
            HUD.Controller.vertexShader
            HUD.Controller.fragmentShader
            HUD.Controller.controllerMeshDown
            (HUD.Controller.controllerUnif canvasDimensions
                (if gameData.controller.downButtonDown then
                    1.0

                 else
                    0.5
                )
            )
        ]


update : Msg -> GameData -> ( GameData, Cmd Msg )
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
                    , user = gameData.user
                    , hero = Nothing
                    }
            in
            ( gameData
            , Task.perform (always (TransitionToInGameLoaderMsg newGameLoaderData)) (Task.succeed ())
            )

        RecvServerMsg message ->
            let
                msgEarth =
                    { rotationAroundSun = message.earth.rotationAroundSun
                    , rotationAroundAxis = message.earth.rotationAroundAxis
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
            ( newGameData
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
            ( newGameData
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
                    if updatedGameData.refreshed == False then
                        Task.attempt ViewportMsg (getViewportOf "webgl-canvas")

                    else
                        Cmd.none

                newGameData =
                    { updatedGameData
                        | renderData = newRenderData
                        , refreshed = True
                    }
            in
            ( newGameData
            , cmd
            )

        PointerEventMsg event ->
            case event of
                MouseUp struct ->
                    let
                        newController =
                            HUD.Controller.handleUp gameData.controller

                        newGameData =
                            { gameData | controller = newController }
                    in
                    ( newGameData, Cmd.none )

                MouseDown struct ->
                    let
                        newController =
                            HUD.Controller.handleDown
                                gameData.controller
                                struct.offsetPos
                                gameData.canvasDimensions

                        newGameData =
                            { gameData | controller = newController }
                    in
                    ( newGameData, Cmd.none )

                MouseMove struct ->
                    let
                        ( newController, newCamera ) =
                            HUD.Controller.handleMove
                                gameData.controller
                                gameData.camera
                                struct.offsetPos

                        newGameData =
                            { gameData
                                | controller = newController
                                , camera = newCamera
                            }
                    in
                    ( newGameData
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
                                , dragState = HUD.Controller.NoDrag
                            }

                        newGameData =
                            { gameData | controller = newController }
                    in
                    ( newGameData
                    , Cmd.none
                    )

                TouchDown struct ->
                    case List.head struct.touches of
                        Nothing ->
                            ( gameData, Cmd.none )

                        Just x ->
                            let
                                newController =
                                    HUD.Controller.handleDown
                                        gameData.controller
                                        x.clientPos
                                        gameData.canvasDimensions

                                newGameData =
                                    { gameData | controller = newController }
                            in
                            ( newGameData
                            , Cmd.none
                            )

                TouchMove struct ->
                    case List.head struct.touches of
                        Nothing ->
                            ( gameData, Cmd.none )

                        Just x ->
                            let
                                ( newController, newCamera ) =
                                    HUD.Controller.handleMove
                                        gameData.controller
                                        gameData.camera
                                        x.clientPos

                                newGameData =
                                    { gameData
                                        | controller = newController
                                        , camera = newCamera
                                    }
                            in
                            ( newGameData
                            , Cmd.none
                            )

        ResizeMsg ->
            ( gameData, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

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
            ( newGameData
            , Cmd.none
            )

        OverviewToggleMsg ->
            let
                newGameData =
                    { gameData | overviewToggle = not gameData.overviewToggle }
            in
            ( newGameData
            , Cmd.none
            )
        TransitionToInGameLoaderMsg _ ->
            ( gameData
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


updateGameData : Float -> Float -> Float -> Float -> Earth -> Earth -> GameData -> GameData
updateGameData elapsed previousElapsed msgElapsed previousMsgElapsed msgEarth previousMsgEarth gameData =
    let
        timeInBetween =
            elapsed - previousElapsed

        weight =
            (elapsed - previousMsgElapsed)
                / (msgElapsed - previousMsgElapsed)

        weightedAve p1 p2 w =
            p1 + w * (p2 - p1)

        newEarth =
            { rotationAroundAxis = weightedAve previousMsgEarth.rotationAroundAxis msgEarth.rotationAroundAxis weight
            , rotationAroundSun = weightedAve previousMsgEarth.rotationAroundSun msgEarth.rotationAroundSun weight
            }

        newPowerChange =
            if gameData.controller.upButtonDown then
                0.0005

            else if gameData.controller.downButtonDown then
                -0.0005

            else
                0

        newPower =
            max 0
                (min 2
                    (gameData.hero.power
                        + (timeInBetween * newPowerChange)
                    )
                )

        newAltitudeChange =
            (newPower - 1) / 50

        newAltitude =
            max 105
                (min 500
                    (gameData.hero.altitude
                        + (timeInBetween * newAltitudeChange)
                    )
                )

        newLongitude =
            gameData.hero.longitude - timeInBetween * gameData.hero.lonSpeed

        newLatitude =
            gameData.hero.longitude - timeInBetween * gameData.hero.latSpeed

        newRotationTheta =
            sin (elapsed / 1000) / 20

        hero =
            gameData.hero

        newHero =
            { hero
                | rotationTheta = newRotationTheta
                , longitude = newLongitude
                , latitude = newLatitude
                , power = newPower
                , altitude = newAltitude
            }

        newGameData =
            { gameData
                | hero = newHero
                , earth = newEarth
            }
    in
    newGameData


fpsOverlay : RenderData -> Html Msg
fpsOverlay renderData =
    let
        fps =
            round (1000 / (renderData.elapsed - renderData.previousElapsed))
                |> String.fromInt
    in
    div
        [ id "fps-overlay" ]
        [ span
            []
            [ text ("FPS: " ++ fps)
            ]
        ]


overviewToggleOverlay : Bool -> Html Msg
overviewToggleOverlay isOn =
    let
        className =
            if isOn then
                "overview-toggle-on"

            else
                "overview-toggle-off"
    in
    div
        [ id "overview-toggle-overlay"
        , class (String.append className " noselect")
        , onMouseDown OverviewToggleMsg
        ]
        [ div
            []
            [ text "Overview" ]
        ]


debugOverlay : GameData -> Html Msg
debugOverlay gameData =
    let
        message =
            ""

        -- (Debug.toString gameData.earth)
        -- |> String.append (Debug.toString gameData.connectionData.earth)
    in
    div
        [ id "debug-overlay"
        , class "noselect"
        ]
        [ div
            []
            [ text message ]
        ]

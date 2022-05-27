module States.InGame exposing (Msg(..), init, subscriptions, update, view)

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onAnimationFrame, onResize)
import Communication.Messenger as Messenger
import Communication.Types exposing (Connection, User)
import HUD.Controller
import HUD.Page exposing (embedInCanvas)
import HUD.Types exposing (Canvas, CanvasDimensions, RenderData)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onMouseDown)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import List
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Platform.Cmd
import Platform.Sub
import Task
import Time
import WebGL exposing (Mesh)
import World.Types exposing (Data, DragState(..), Earth, Hero, MeshList, Vertex, World)
import World.World as World


type Msg
    = TimeElapsed Time.Posix
    | ResizeMsg
    | PointerEventMsg PointerEvent
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | RecvServerMsg Messenger.RecvServerValue
    | RecvServerMsgError String
    | UpdateTimeMsg Time.Posix
    | OverviewToggleMsg
    | TransitionToInGameLoaderMsg


type PointerEvent
    = MouseUp Mouse.Event
    | MouseDown Mouse.Event
    | MouseMove Mouse.Event
    | TouchMove Touch.Event
    | TouchDown Touch.Event
    | TouchUp Touch.Event


type alias InitData =
    { connection : Connection
    , canvas : Canvas
    , world : World
    }


init : RenderData -> Connection -> CanvasDimensions -> Hero -> ( InitData, Cmd Msg )
init renderData connection canvasDim hero =
    let
        canvas =
            { canvasDim = canvasDim
            , renderData = renderData
            , overviewToggle = False
            }

        world =
            { earth = connection.earth.msgEarth
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
            , hero = hero
            }
    in
    ( { connection = connection, canvas = canvas, world = world }
    , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
    )


subscriptions : Sub Msg
subscriptions =
    Platform.Sub.batch
        [ onAnimationFrame (\x -> TimeElapsed x)
        , onResize (\width height -> ResizeMsg)
        , Messenger.messageReceiver recvServerJson
        ]


view : Data -> User -> Canvas -> World -> Connection -> Html Msg
view data user canvas world connection =
    let
        earth =
            world.earth

        controller =
            world.controller

        renderData =
            canvas.renderData

        canvasDim =
            canvas.canvasDim

        camera =
            world.camera

        hero =
            world.hero

        earthMesh =
            data.earthMesh

        overviewToggle =
            canvas.overviewToggle

        containerAttrs =
            if overviewToggle then
                [ class "background-black" ]

            else
                []
    in
    embedInCanvas
        containerAttrs
        [ fpsOverlay renderData
        , overviewToggleOverlay overviewToggle
        , debugOverlay ""
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
            (World.heroMesh hero.envColor)
            (World.heroUnif overviewToggle canvasDim earth hero camera)
        , WebGL.entity
            World.vertexShader
            World.fragmentShader
            World.fireMesh
            (World.fireUnif overviewToggle canvasDim earth hero camera)
        , WebGL.entity
            World.vertexShader
            World.fragmentShader
            earthMesh
            (World.earthUnif overviewToggle canvasDim earth hero camera)
        , WebGL.entity
            World.vertexShader
            World.fragmentShader
            World.axisMesh
            (World.axisUnif overviewToggle canvasDim earth hero camera)
        , WebGL.entity
            World.vertexShader
            World.fragmentShader
            World.sunMesh
            (World.sunUnif overviewToggle canvasDim earth hero camera)
        , WebGL.entity
            HUD.Controller.vertexShader
            HUD.Controller.fragmentShader
            HUD.Controller.controllerMeshUp
            (HUD.Controller.controllerUnif canvasDim
                (if controller.upButtonDown then
                    1.0

                 else
                    0.5
                )
            )
        , WebGL.entity
            HUD.Controller.vertexShader
            HUD.Controller.fragmentShader
            HUD.Controller.controllerMeshDown
            (HUD.Controller.controllerUnif canvasDim
                (if controller.downButtonDown then
                    1.0

                 else
                    0.5
                )
            )
        ]


type alias UpdateData =
    { connection : Connection
    , canvas : Canvas
    , world : World
    , data : Data
    }


update : Msg -> UpdateData -> ( UpdateData, Cmd Msg )
update msg values =
    case msg of
        RecvServerMsgError message ->
            ( values
            , Task.perform (always TransitionToInGameLoaderMsg) (Task.succeed ())
            )

        RecvServerMsg message ->
            let
                connection =
                    values.connection

                msgEarth =
                    { rotationAroundSun = message.earth.rotationAroundSun
                    , rotationAroundAxis = message.earth.rotationAroundAxis
                    }

                newEarth =
                    { msgEarth = msgEarth
                    , previousMsgEarth =
                        connection.earth.msgEarth
                    , previousEarthAtMsg =
                        values.world.earth
                    }

                newConnection =
                    { connection | earth = newEarth }
            in
            ( { values | connection = newConnection }
            , Task.perform UpdateTimeMsg Time.now
            )

        UpdateTimeMsg dt ->
            let
                connection =
                    values.connection

                msgElapsed =
                    toFloat (Time.posixToMillis dt)

                newElapsedData =
                    { msgElapsed = msgElapsed
                    , previousMsgElapsed =
                        connection.elapsed.msgElapsed
                    }

                newConnection =
                    { connection | elapsed = newElapsedData }
            in
            ( { values | connection = newConnection }
            , Cmd.none
            )

        TimeElapsed dt ->
            let
                canvas =
                    values.canvas

                renderData =
                    canvas.renderData

                connection =
                    values.connection

                earthData =
                    connection.earth

                elapsed =
                    toFloat (Time.posixToMillis dt)

                previousElapsed =
                    renderData.elapsed

                elapsedData =
                    connection.elapsed

                world =
                    values.world

                newRenderData =
                    { elapsed = elapsed
                    , previousElapsed = previousElapsed
                    }

                newCanvas =
                    { canvas | renderData = newRenderData }

                newWorld =
                    updateWorld
                        values.data.serverUpdateInterval
                        elapsed
                        previousElapsed
                        earthData.msgEarth
                        earthData.previousEarthAtMsg
                        world
            in
            ( { values | world = newWorld, canvas = newCanvas }
            , Cmd.none
            )

        PointerEventMsg event ->
            case event of
                MouseUp struct ->
                    let
                        world =
                            values.world

                        newController =
                            HUD.Controller.handleUp world.controller

                        newWorld =
                            { world | controller = newController }
                    in
                    ( { values | world = newWorld }
                    , Cmd.none
                    )

                MouseDown struct ->
                    let
                        world =
                            values.world

                        canvas =
                            values.canvas

                        newController =
                            HUD.Controller.handleDown
                                world.controller
                                struct.offsetPos
                                canvas.canvasDim

                        newWorld =
                            { world | controller = newController }
                    in
                    ( { values | world = newWorld }
                    , Cmd.none
                    )

                MouseMove struct ->
                    let
                        world =
                            values.world

                        ( newController, newCamera ) =
                            HUD.Controller.handleMove
                                world.controller
                                world.camera
                                struct.offsetPos

                        newWorld =
                            { world
                                | controller = newController
                                , camera = newCamera
                            }
                    in
                    ( { values | world = newWorld }
                    , Cmd.none
                    )

                TouchUp struct ->
                    let
                        world =
                            values.world

                        controller =
                            world.controller

                        newController =
                            { controller
                                | upButtonDown = False
                                , downButtonDown = False
                                , dragState = World.Types.NoDrag
                            }

                        newWorld =
                            { world | controller = newController }
                    in
                    ( { values | world = newWorld }
                    , Cmd.none
                    )

                TouchDown struct ->
                    case List.head struct.touches of
                        Nothing ->
                            ( values, Cmd.none )

                        Just x ->
                            let
                                world =
                                    values.world

                                canvas =
                                    values.canvas

                                newController =
                                    HUD.Controller.handleDown
                                        world.controller
                                        x.clientPos
                                        canvas.canvasDim

                                newWorld =
                                    { world | controller = newController }
                            in
                            ( { values | world = newWorld }
                            , Cmd.none
                            )

                TouchMove struct ->
                    case List.head struct.touches of
                        Nothing ->
                            ( values, Cmd.none )

                        Just x ->
                            let
                                world =
                                    values.world

                                ( newController, newCamera ) =
                                    HUD.Controller.handleMove
                                        world.controller
                                        world.camera
                                        x.clientPos

                                newWorld =
                                    { world
                                        | controller = newController
                                        , camera = newCamera
                                    }
                            in
                            ( { values | world = newWorld }
                            , Cmd.none
                            )

        ResizeMsg ->
            ( values
            , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
            )

        ViewportMsg returnValue ->
            let
                canvas =
                    values.canvas

                newCanvasDim =
                    returnValue
                        |> Result.map .viewport
                        |> Result.map
                            (\v ->
                                { width = round v.width
                                , height = round v.height
                                }
                            )
                        |> Result.withDefault canvas.canvasDim

                newCanvas =
                    { canvas | canvasDim = newCanvasDim }
            in
            ( { values | canvas = newCanvas }
            , Cmd.none
            )

        OverviewToggleMsg ->
            let
                canvas =
                    values.canvas

                newCanvas =
                    { canvas | overviewToggle = not canvas.overviewToggle }
            in
            ( { values | canvas = newCanvas }
            , Cmd.none
            )

        _ ->
            ( values
            , Cmd.none
            )



-- Some helpers.


recvServerJson : String -> Msg
recvServerJson value =
    case Messenger.decodeJson value of
        Ok result ->
            RecvServerMsg result

        Err errorMessage ->
            RecvServerMsgError "Error while communicating with the server"


interpolate : Float -> Float -> Float -> Float -> Float
interpolate from current to stepFraction =
    let
        direction =
            if to - current /= 0 then
                (to - current) / abs (to - current)

            else
                0

        distance =
            abs (to - from)

        step =
            stepFraction * direction * distance
    in
    current + step


updateWorld : Int -> Float -> Float -> Earth -> Earth -> World -> World
updateWorld serverUpdateInterval elapsed previousElapsed msgEarth previousEarthAtMsg world =
    let
        hero =
            world.hero

        earth =
            world.earth

        controller =
            world.controller

        timeInBetween =
            elapsed - previousElapsed

        stepFraction =
            timeInBetween / toFloat serverUpdateInterval

        newRotationAroundAxis =
            interpolate
                previousEarthAtMsg.rotationAroundAxis
                earth.rotationAroundAxis
                msgEarth.rotationAroundAxis
                stepFraction

        newRotationAroundSun =
            interpolate
                previousEarthAtMsg.rotationAroundSun
                earth.rotationAroundSun
                msgEarth.rotationAroundSun
                stepFraction

        newEarth =
            { rotationAroundAxis = newRotationAroundAxis
            , rotationAroundSun = newRotationAroundSun
            }

        newPowerChange =
            if controller.upButtonDown then
                0.0005

            else if controller.downButtonDown then
                -0.0005

            else
                0

        newPower =
            max 0
                (min 2
                    (hero.power
                        + (timeInBetween * newPowerChange)
                    )
                )

        newAltitudeChange =
            (newPower - 1) / 50

        newAltitude =
            max 105
                (min 500
                    (hero.altitude
                        + (timeInBetween * newAltitudeChange)
                    )
                )

        newLongitude =
            hero.longitude - timeInBetween * hero.lonSpeed

        newLatitude =
            hero.longitude - timeInBetween * hero.latSpeed

        newRotationTheta =
            sin (elapsed / 1000) / 20

        newHero =
            { hero
                | rotationTheta = newRotationTheta
                , longitude = newLongitude
                , latitude = newLatitude
                , power = newPower
                , altitude = newAltitude
            }
    in
    { world | hero = newHero, earth = newEarth }


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


debugOverlay message =
    div
        [ id "debug-overlay"
        , class "noselect"
        ]
        [ div
            []
            [ text message ]
        ]

module Main exposing (main)

import World exposing (heroMesh, 
                       fireMesh,
                       heroUnif,
                       fireUnif,
                       axisMesh, 
                       axisUnif,
                       earthUnif, 
                       sunMesh, 
                       sunUnif)

import Controller exposing (controllerMeshUp, 
                            controllerMeshDown, 
                            controllerUnif, 
                            coordinatesWithinUpButton, 
                            coordinatesWithinDownButton)

import Common exposing (Model, 
                        GameState(..), 
                        ConnectionState(..), 
                        DragState(..),
                        GameData,
                        ConnectionData,
                        RenderData,
                        CanvasDimensions,
                        Vertex,
                        viewportSize, 
                        vertexShader, 
                        fragmentShader
                        )

import ObjLoader
import Receiver
import Flags

import Task
import Time
import List

import Browser
import Browser.Dom exposing (getViewportOf, Viewport)
import Browser.Events exposing (onAnimationFrame, onResize)

import Platform.Sub
import Platform.Cmd

import Html exposing (Html, div, text, button, p, span)
import Html.Attributes exposing (height, style, width, id, class)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch

import Http

import Json.Decode

import Length exposing (Meters, meters)
import Obj.Decode exposing (expectObj)

import Math.Vector3 as Vec3 exposing (vec3)

import WebGL exposing (Mesh)



type PointerEvent = 
    MouseUp Mouse.Event
  | MouseDown Mouse.Event
  | MouseMove Mouse.Event
  | TouchMove Touch.Event
  | TouchDown Touch.Event
  | TouchUp Touch.Event


type Msg = TimeElapsed Time.Posix
  | ResizeMsg
  | PointerEventMsg PointerEvent
  | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
  | RecvServerMsg Receiver.RecvServerValue
  | RecvServerMsgError String
  | UpdateTimeMsg Time.Posix
  | StartGameMsg
  | EarthMeshLoaded (Result Http.Error (Mesh Vertex))


-- The model initialization

init : Json.Decode.Value -> (Model, Cmd Msg)
init flagsMsg = 
  let 
      -- Try loading flags from javascript

      flags = Json.Decode.decodeValue Flags.flagsDecoder flagsMsg
      
      -- If not ok, fail fast.
 
      gameState = case flags of Ok value -> MainMenu
                                Err _ -> InitializationFailed

      -- If ok, set earth model url

      modelEarth = case flags of Ok value -> value.modelEarth
                                 Err _ -> ""

      cmd = 
        case flags of 
          Ok value -> Http.get { url = modelEarth
                               , expect = (expectObj 
                                           EarthMeshLoaded 
                                           meters 
                                           ObjLoader.earthMeshDecoder) 
                               }
          Err _ -> Cmd.none



  in
    ( { gameState = gameState
      , connectionState = Disconnected
      , data = { earthMesh = Nothing } 
      }
    , cmd )


-- The view function


view : Model -> Html Msg
view model =

  case ( model.gameState
       , model.connectionState
       , model.data.earthMesh) of 

     

    (InitializationFailed, _, _) -> 
      div [] [ text "Initialization failed"]

    (MainMenu, _, _) -> 
      div [ class "main-menu-container" ] 
          [ p [] [ text "So it begins (the grand hot air balloon adventure)" ]
          , button [ onClick StartGameMsg ] [ text "Start here" ] ]

    (FlightMode gameData, Connected connectionData, Just earthMesh) ->
      case (gameData.earth, gameData.renderData, gameData.canvasDimensions) of
        (Just earth, Just renderData, Nothing) ->
           WebGL.toHtml [ width (Tuple.first viewportSize)
                        , height (Tuple.second viewportSize)
                        , style "display" "block"
                        , style "height" "100vh"
                        , style "width" "100vw"
                        , id "webgl-canvas" ]
                        [ ]

        (Just earth, Just renderData, Just canvasDimensions) ->
          let fps = (case renderData.elapsedPrevious of
                       Just elapsedPrevious ->
                         (String.fromInt <| round (1000 / (renderData.elapsed - 
                                                           elapsedPrevious)))
                       Nothing -> 
                         "-" )
              camera = gameData.camera
              hero = gameData.hero
          in

          div 
          [ id "canvas-container" ] 
          [ div 
            [ id "fps-overlay" ] 
            [ span 
              [] 
              [ text ("FPS: " ++ fps)
              ]
            ] 
           , WebGL.toHtml [ width (Tuple.first viewportSize)
                          , height (Tuple.second viewportSize)
                          , style "display" "block"
                          , style "height" "100vh"
                          , style "width" "100vw"
                          , id "webgl-canvas"
                          , Touch.onEnd (PointerEventMsg << TouchUp)
                          , Touch.onStart (PointerEventMsg << TouchDown)
                          , Touch.onMove (PointerEventMsg << TouchMove)
                          , Mouse.onUp (PointerEventMsg << MouseUp)
                          , Mouse.onDown (PointerEventMsg << MouseDown)
                          , Mouse.onMove (PointerEventMsg << MouseMove) ]
                          [ (WebGL.entity
                            vertexShader
                            fragmentShader
                            heroMesh
                            (heroUnif canvasDimensions earth hero camera))
                          , (WebGL.entity
                            vertexShader
                            fragmentShader
                            fireMesh
                            (fireUnif canvasDimensions earth hero camera))
                          , (WebGL.entity
                            vertexShader
                            fragmentShader
                            earthMesh
                            (earthUnif canvasDimensions earth hero camera))
                          , (WebGL.entity
                            vertexShader
                            fragmentShader
                            axisMesh
                            (axisUnif canvasDimensions earth hero camera))
                          , (WebGL.entity
                            vertexShader
                            fragmentShader
                            sunMesh
                            (sunUnif canvasDimensions earth hero camera))
                          , (WebGL.entity
                            vertexShader
                            fragmentShader
                            controllerMeshUp
                            (controllerUnif gameData (if gameData.controller.upButtonDown then 1.0 else 0.5)))
                          , (WebGL.entity
                            vertexShader
                            fragmentShader controllerMeshDown
                            (controllerUnif gameData (if gameData.controller.downButtonDown then 1.0 else 0.5)))
                          ] ]
        (_, _, _) ->
          div [] [
               text (Debug.toString (gameData.canvasDimensions))
              ]

    (FlightMode _, _, _) -> 
      div [] [ text "Starting.." ]


-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ = 
  Platform.Sub.batch 
    [ (onAnimationFrame (\x -> TimeElapsed x))
    , (onResize (\width height -> ResizeMsg))
    , (Receiver.messageReceiver recvServerJson) ]


-- Updates


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 

    StartGameMsg ->
      case model.connectionState of 

        Disconnected -> 
          ( model, Cmd.none )

        Connected connectionData ->
          let gameData = 
                { earth = Nothing
                , camera = 
                   { azimoth = 0
                   , elevation = 0 }
                , controller = 
                  { dragState = NoDrag
                  , pointerOffset = { x = 0, y = 0 }
                  , previousOffset = { x = 0, y = 0 }
                  , upButtonDown = False
                  , downButtonDown = False } 
                , hero = 
                   { altitude = 11
                   , latitude = -0.3
                   , longitude = 0.0
                   , rotationTheta = 0
                   , power = 1 } 
                , canvasDimensions = Nothing
                , renderData = Nothing
                }
          in
            case connectionData.earth of
              Just earthData ->
                ( { model | gameState = FlightMode ( { gameData | earth = Just earthData.msgEarth } ) }
                  , Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )
              Nothing -> 
                ( { model | gameState = FlightMode gameData }
                , Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )
 
    RecvServerMsgError message ->

      case model.connectionState of 

        Connected connectionData ->
          ( { model | connectionState = Disconnected }
          , Cmd.none )

        Disconnected ->
          ( model, Cmd.none )

    RecvServerMsg message ->
      let msgEarth =
            { locationX = message.earth.locationX
            , locationY = message.earth.locationY
            , locationZ = message.earth.locationZ
            , rotationTheta = message.earth.rotationTheta }
      in
        case model.connectionState of 
          Connected connectionData ->
            case connectionData.earth of 

              Just earthData ->
                let newEarth = 
                      { msgEarth = msgEarth 
                      ,  previousMsgEarth = earthData.msgEarth }
                    newConnectionData = 
                      { connectionData | earth = Just newEarth }
                in
                  ( { model | connectionState = Connected newConnectionData }
                  , Task.perform UpdateTimeMsg Time.now )

              Nothing -> 
                let newEarth = 
                      { msgEarth = msgEarth 
                      , previousMsgEarth = msgEarth }
                    newConnectionData = 
                      { connectionData | earth = Just newEarth }
                in
                  ( { model | connectionState = Connected newConnectionData }
                  , Task.perform UpdateTimeMsg Time.now )

          Disconnected ->
            let newConnectionData = 
                  { earth =
                      Just { msgEarth = msgEarth
                           , previousMsgEarth = msgEarth }
                  , elapsed = Nothing }
            in 
                  ( { model | connectionState = Connected newConnectionData }
                  , Task.perform UpdateTimeMsg Time.now )


    UpdateTimeMsg dt ->

      case model.connectionState of 
        Connected connectionData ->
          let msgElapsed = toFloat (Time.posixToMillis dt)
          in
            case connectionData.elapsed of 
              Just elapsedData ->
                let
                  newElapsedData = 
                    { msgElapsed = msgElapsed
                    , previousMsgElapsed = Just elapsedData.msgElapsed
                    }
                  newConnectionData = 
                    { connectionData | elapsed = Just newElapsedData }
                in
                  ( { model | connectionState = Connected newConnectionData }
                  , Cmd.none )

              Nothing ->
                let
                  newElapsedData = 
                    { msgElapsed = msgElapsed
                    , previousMsgElapsed = Just msgElapsed
                    }
                  newConnectionData = 
                    { connectionData | elapsed = Just newElapsedData }
                in 
                  ( { model | connectionState = Connected newConnectionData }
                  , Cmd.none )

        Disconnected ->
          ( model, Cmd.none )
          

    TimeElapsed dt ->
      case model.gameState of 
        MainMenu ->
          ( model, Cmd.none )

        InitializationFailed ->
          ( model, Cmd.none )

        FlightMode gameData ->
          let elapsed = toFloat (Time.posixToMillis dt)
          in
            case gameData.renderData of
              Nothing -> 
                let newRenderData = { elapsed = elapsed
                                    , elapsedPrevious = Nothing }
                    newGameData = { gameData | renderData = Just newRenderData }
                in
                  ( { model | gameState = FlightMode newGameData }
                  , Cmd.none )
              Just renderData ->
                case renderData.elapsedPrevious of
                  Nothing -> 
                    let 
                      newRenderData = { elapsed = elapsed
                                      , elapsedPrevious = Just renderData.elapsed }
                      newGameData = { gameData | renderData = Just newRenderData }
                    in 
                     ( { model | gameState = FlightMode newGameData }
                     , Cmd.none )
                  Just elapsedPrevious ->
                    let
                      newRenderData = { elapsed = elapsed
                                      , elapsedPrevious = Just renderData.elapsed }

                      timeInBetween = elapsed - elapsedPrevious
                    in
                      case model.connectionState of 
                        Disconnected ->
                          let newGameData = { gameData | renderData = Just newRenderData }
                          in
                            ( { model | gameState = FlightMode newGameData }
                            , Cmd.none )
                        Connected connectionData ->
                          case (connectionData.earth, connectionData.elapsed) of
                            (Just earthData, Just elapsedData) ->
                              case elapsedData.previousMsgElapsed of
                                Just previousMsgElapsed ->
                                  let
                                    msgElapsed = elapsedData.msgElapsed
                                    msgEarth = earthData.msgEarth
                                    previousMsgEarth = earthData.previousMsgEarth

                                    weight = ((elapsed - previousMsgElapsed) / 
                                              (msgElapsed - previousMsgElapsed))

                                    weightedAve p1 p2 w = 
                                      p1 + w * (p2 - p1)

                                    newEarth = 
                                      { rotationTheta = weightedAve previousMsgEarth.rotationTheta msgEarth.rotationTheta weight
                                      , locationX = weightedAve previousMsgEarth.locationX msgEarth.locationX weight
                                      , locationY = weightedAve previousMsgEarth.locationY msgEarth.locationY weight
                                      , locationZ = weightedAve previousMsgEarth.locationZ msgEarth.locationZ weight
                                      }

                                    newPowerChange = (if gameData.controller.upButtonDown then 0.0001
                                                      else (if gameData.controller.downButtonDown then -0.0001 else 0))

                                    newPower = max 0 (min 2 (gameData.hero.power + 
                                                             (timeInBetween * newPowerChange)))

                                    newAltitudeChange = (newPower - 1) / 200
                                    newAltitude = max 10.5 (min 100 (gameData.hero.altitude + 
                                                                     (timeInBetween*newAltitudeChange)))

                                    newLongitude = gameData.hero.longitude - timeInBetween * 0.00005

                                    newRotationTheta = sin (elapsed / 1000) / 20

                                    hero = gameData.hero
                                    newHero = { hero | rotationTheta = newRotationTheta, 
                                                       longitude = newLongitude,
                                                       power = newPower,
                                                       altitude = newAltitude } 
                                    newGameData = { gameData | hero = newHero
                                                             , renderData = Just newRenderData
                                                             , earth = Just newEarth
                                                  }

                                  in
                                    ( { model | gameState = FlightMode newGameData } 
                                    , Cmd.none 
                                    )


                                Nothing ->
                                  let newGameData = { gameData | renderData = Just newRenderData }
                                  in
                                    ( { model | gameState = FlightMode newGameData }
                                    , Cmd.none )
                            (_, _) ->
                              let newGameData = { gameData | renderData = Just newRenderData }
                              in
                                ( { model | gameState = FlightMode newGameData }
                                , Cmd.none )

    -- Here mouse and touch related events are handled

    PointerEventMsg event -> 
      case model.gameState of 

        MainMenu ->
          ( model, Cmd.none )

        InitializationFailed ->
          ( model, Cmd.none )

        FlightMode gameData ->

          case event of 

            MouseUp struct ->
              let controller = 
                    gameData.controller

                  newController = 
                    { controller | upButtonDown = False,
                                   downButtonDown = False,
                                   dragState = NoDrag }

                  newGameData = { gameData | controller = newController }
              in 
                ( { model | gameState = FlightMode newGameData }, Cmd.none)
    
            MouseDown struct ->
              let offsetPos = 
                    struct.offsetPos
                  coordsInUp = 
                    coordinatesWithinUpButton gameData offsetPos
                  coordsInDown = 
                    coordinatesWithinDownButton gameData offsetPos
                  upButtonDown = 
                    if coordsInUp then True else False
                  downButtonDown = 
                    if coordsInDown then True else False
                  controller = 
                    gameData.controller
                  newController = 
                    { controller | previousOffset = { x = round (Tuple.first offsetPos),
                                                      y = round (Tuple.second offsetPos)},
                                   pointerOffset = { x = round (Tuple.first offsetPos),
                                                     y = round (Tuple.second offsetPos)},
                                   upButtonDown = upButtonDown,
                                   downButtonDown = downButtonDown,
                                   dragState = Drag }
                  newGameData = { gameData | controller = newController }
              in
                ( { model | gameState = FlightMode newGameData }, Cmd.none )
    
            MouseMove struct ->
              let offsetPos = struct.offsetPos

                  newAzimoth = 
                    (if gameData.controller.dragState == Drag 
                     then gameData.camera.azimoth - (toFloat (round (Tuple.first offsetPos) - 
                                                              gameData.controller.previousOffset.x)) * pi / 180
                     else gameData.camera.azimoth)

                  newElevation = 
                    (if gameData.controller.dragState == Drag 
                     then gameData.camera.elevation + (toFloat (round (Tuple.second offsetPos) - 
                                                                gameData.controller.previousOffset.y)) * pi / 180
                     else gameData.camera.elevation)
                  camera = 
                    gameData.camera
                  newCamera = 
                    { camera | azimoth = newAzimoth,
                               elevation = ( if newElevation <= (4*pi/10) 
                                             then ( if newElevation >= (-4*pi/10) 
                                                    then newElevation else gameData.camera.elevation)
                                             else gameData.camera.elevation ) }
                  controller = 
                    gameData.controller
                  newController = 
                    { controller | previousOffset = { x = round (Tuple.first offsetPos),
                                                      y = round (Tuple.second offsetPos) } }
                  newGameData = {gameData | controller = newController 
                                          , camera = newCamera }

    
              in ( { model | gameState = FlightMode newGameData
                   }, Cmd.none )
    
            TouchUp struct ->
              let controller = 
                    gameData.controller
                  newController = 
                    { controller | upButtonDown = False,
                                   downButtonDown = False,
                                   dragState = NoDrag }
                  newGameData = { gameData | controller = newController }
              in 
                ( { model | gameState = FlightMode newGameData }, Cmd.none)
    
            TouchDown struct ->
              case (List.head struct.touches) of 

                Nothing -> (model, Cmd.none)

                Just x -> 
                  let offsetPos = 
                        x.clientPos
                      coordsInUp = 
                        coordinatesWithinUpButton gameData offsetPos
                      coordsInDown = 
                        coordinatesWithinDownButton gameData offsetPos
                      upButtonDown = 
                        if coordsInUp then True else False
                      downButtonDown = 
                        if coordsInDown then True else False
                      controller = 
                        gameData.controller
                      newController = 
                        { controller | previousOffset = { x = round (Tuple.first offsetPos),
                                                          y = round (Tuple.second offsetPos)},
                                       pointerOffset = { x = round (Tuple.first offsetPos),
                                                         y = round (Tuple.second offsetPos)},
                                       upButtonDown = upButtonDown,
                                       downButtonDown = downButtonDown,
                                       dragState = Drag }
                      newGameData = { gameData | controller = newController }
                  in
                    ( { model | gameState = FlightMode gameData }, Cmd.none )
    
            TouchMove struct ->
              case (List.head struct.touches) of 

                Nothing -> (model, Cmd.none)

                Just x ->
                  let offsetPos = 
                        x.clientPos

                      newAzimoth = 
                        (if gameData.controller.dragState == Drag 
                         then gameData.camera.azimoth - (toFloat (round (Tuple.first offsetPos) - 
                                                                   gameData.controller.previousOffset.x)) * pi / 180
                         else gameData.camera.azimoth)

                      newElevation = 
                        (if gameData.controller.dragState == Drag 
                         then gameData.camera.elevation + (toFloat (round (Tuple.second offsetPos) - 
                                                                    gameData.controller.previousOffset.y)) * pi / 180
                         else gameData.camera.elevation)

                      camera = 
                        gameData.camera

                      newCamera = 
                         { camera | azimoth = newAzimoth,
                                    elevation =
                                      ( if newElevation <= (4*pi/10) 
                                        then (if newElevation >= (-4*pi/10) 
                                              then newElevation else gameData.camera.elevation)
                                        else gameData.camera.elevation ) }

                      controller =
                        gameData.controller

                      newController = 
                        { controller | previousOffset = { x = round (Tuple.first offsetPos),
                                                          y = round (Tuple.second offsetPos) } }
                      newGameData = 
                        { gameData | controller = newController
                                   , camera = newCamera }
    
                  in ( { model | gameState = FlightMode newGameData 
                       }, Cmd.none )


    ResizeMsg -> 
      (model, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") ) 

    -- This Msg is often induced by code also, as it refreshes the screen..

    ViewportMsg returnValue -> 
      case returnValue of
        Ok struct ->
          case model.gameState of
            FlightMode gameData ->
              let 
                newCanvasDimensions = { width = round struct.viewport.width
                                      , height = round struct.viewport.height }
                newGameData = { gameData | canvasDimensions = Just newCanvasDimensions }
              in 
              ({ model | gameState = FlightMode newGameData }, 
               Cmd.none)

            MainMenu ->  
              (model, Cmd.none )

            InitializationFailed ->  
              (model, Cmd.none )

        Err errMsg -> (model, Cmd.none)

    -- Load meshes

    EarthMeshLoaded result -> 
      let data = model.data
          newData = { data | earthMesh = (Result.toMaybe result) }
         
      in ( { model | data = newData }
         , Task.attempt ViewportMsg (getViewportOf "webgl-canvas") 
         )


-- Here it begins.

main : Program Json.Decode.Value Model Msg
main =
  Browser.element { init = init
                  , view = view
                  , subscriptions = subscriptions
                  , update = update }


-- Some helpers. This handles server message decoding.

recvServerJson : String -> Msg
recvServerJson value =
  case Receiver.decodeJson value of
    Ok result ->
      RecvServerMsg result
    Err errorMessage ->
      RecvServerMsgError ("Error while communicating with the server")

-- As we use full screen in the game and the browser window sizes differ,
-- scale to common space.

fixOffset : { x : Int , y: Int } -> { width: Int, height: Int } -> { x : Int, y : Int }
fixOffset offset viewport = { x = round (toFloat (offset.x * (Tuple.first viewportSize)) / 
                                         (toFloat viewport.width)),
                              y = round (toFloat (offset.y * (Tuple.second viewportSize)) / 
                                         (toFloat viewport.height)) }


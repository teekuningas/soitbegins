module Main exposing (main)

import World exposing (heroMesh, fireMesh,
                       heroUnif, fireUnif,
                       axisMesh, axisUnif,
                       earthUnif, 
                       sunMesh, sunUnif)

import Controller exposing (controllerMeshUp, controllerMeshDown, controllerUnif, 
                            coordinatesWithinUpButton, coordinatesWithinDownButton)

import Common exposing (Model, GameState(..), ConnectionState(..), DragState(..),
                        viewportSize, vertexShader, fragmentShader,
                        Vertex)

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


-- Some type definitions

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

      -- If ok, set serverUpdateInterval 

      serverUpdateInterval = case flags of Ok value -> value.serverUpdateInterval
                                           Err _ -> 1000

      -- If ok, set earth model url

      modelEarth = case flags of Ok value -> value.modelEarth
                                 Err _ -> ""

      -- Set some defaults for loading time

      earth = { locationX = 100
              , locationY = 100
              , locationZ = 100
              , rotationTheta = 0 
              , mesh = Nothing }
  in

  ( { hero = { altitude = 11
             , latitude = -0.3
             , longitude = 0.0
             , rotationTheta = 0
             , power = 1 } 
    , earth = earth
    , camera = { azimoth = 0
               , elevation = 0 }
    , updateParams = { msgElapsed = 0
                     , msgElapsedPrevious = 0
                     , msgEarth = earth
                     , msgEarthPrevious = earth
                     , elapsed = 10
                     , elapsedPrevious = 0 
                     , serverUpdateInterval = serverUpdateInterval}
    , canvasDimensions = { width = 0, height = 0 }
    , controller = { dragState = NoDrag
                   , pointerOffset = { x = 0, y = 0 }
                   , previousOffset = { x = 0, y = 0 }
                   , upButtonDown = False
                   , downButtonDown = False } 
    , messages = []
    , gameState = gameState
    , connectionState = Disconnected
    }
  , Cmd.batch [ Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
              , Http.get { url = modelEarth
                         , expect = (expectObj 
                                     EarthMeshLoaded 
                                     meters 
                                     ObjLoader.earthMeshDecoder) }
              ] ) 


-- The view function

view : Model -> Html Msg
view model =
  let
    upButtonDown = model.controller.upButtonDown
    downButtonDown = model.controller.downButtonDown
    connectionState = model.connectionState
    gameState = model.gameState
    maybeMesh = model.earth.mesh
  in
  case (gameState, connectionState, maybeMesh) of 
    (InitializationFailed, _, _) -> 
      div [] [ text "Initialization failed"]
    (MainMenu, _, _) -> 
      div [ class "main-menu-container" ] 
          [ p [] [ text "So it begins (the grand hot air balloon adventure)" ]
          , button [ onClick StartGameMsg ] [ text "Start here" ] ]
    (FlightMode, Connected, Just earthMesh) ->
      div [id "canvas-container"] [ div 
               [ id "fps-overlay" ] 
               [ span [] 
                 [ text ("FPS: " ++ 
                         (String.fromInt <| round (1000 / (model.updateParams.elapsed - 
                                                           model.updateParams.elapsedPrevious))))
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
                              (heroUnif model))
                            , (WebGL.entity
                              vertexShader
                              fragmentShader
                              fireMesh
                              (fireUnif model))
                            , (WebGL.entity
                              vertexShader
                              fragmentShader
                              earthMesh
                              (earthUnif model))
                            , (WebGL.entity
                              vertexShader
                              fragmentShader
                              axisMesh
                              (axisUnif model))
                            , (WebGL.entity
                              vertexShader
                              fragmentShader
                              sunMesh
                              (sunUnif model))
                            , (WebGL.entity
                              vertexShader
                              fragmentShader
                              controllerMeshUp
                              (controllerUnif model (if upButtonDown then 1.0 else 0.5)))
                            , (WebGL.entity
                              vertexShader
                              fragmentShader
                              controllerMeshDown
                              (controllerUnif model (if downButtonDown then 1.0 else 0.5)))
                            ] ]
    (FlightMode, _, _) -> 
      div [] [text (Maybe.withDefault "Starting.." (List.head model.messages)) ]


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

    -- When start game button is clicked, move to flight mode

    StartGameMsg ->
      ( { model | gameState = FlightMode }, 
          Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

    -- If error comes from server, set connection state to disconnected

    RecvServerMsgError message ->
      ( { model | messages = [message] ++ model.messages,
                  connectionState = Disconnected }, Cmd.none )

    -- Receive World parameters through a port.
    -- Store parameters, but do not update yet.

    RecvServerMsg message ->
      let oldEarth = model.updateParams.msgEarth
          newEarth = { oldEarth | locationX = message.earth.locationX,
                                  locationY = message.earth.locationY,
                                  locationZ = message.earth.locationZ,
                                  rotationTheta = message.earth.rotationTheta }


          updateParams = model.updateParams
          newUpdateParams = { updateParams | msgEarth = newEarth,
                                             msgEarthPrevious = oldEarth }
      in
        ( { model | updateParams = newUpdateParams } 
        , Task.perform UpdateTimeMsg Time.now)

    -- For smooth interpolation we need to collect times 
    -- when messages have been received

    UpdateTimeMsg dt ->
      let screenRefresh = if model.connectionState == Disconnected then True else False
          updateParams = model.updateParams

          msgElapsed = toFloat (Time.posixToMillis dt)

          -- If screen refresh, discard the old values

          msgElapsedPrevious = if screenRefresh then msgElapsed 
                               else model.updateParams.msgElapsed
          msgEarthPrevious = if screenRefresh then model.updateParams.msgEarth 
                             else model.updateParams.msgEarthPrevious

          newUpdateParams = { updateParams | msgElapsed = msgElapsed,
                                             msgElapsedPrevious = msgElapsedPrevious,
                                             msgEarthPrevious = msgEarthPrevious }

          cmd = 
            if screenRefresh
            then Task.attempt ViewportMsg (getViewportOf "webgl-canvas") 
            else Cmd.none

      in
        ( { model | updateParams = newUpdateParams,
                    connectionState = Connected }, cmd )

    -- This is called for each animation frame update. 
    -- Here we construct a interpolated world
    -- which is reflected in the visuals

    TimeElapsed dt ->
      ( let 

            -- Update local time parameters

            elapsed = toFloat (Time.posixToMillis dt)
            elapsedPrevious = model.updateParams.elapsed

            updateParams = model.updateParams
            newUpdateParams = { updateParams | elapsed = elapsed,
                                               elapsedPrevious = elapsedPrevious }

            timeInBetween = elapsed - elapsedPrevious

            -- Interpolate between two received earth messages.
            -- This may need some rethinking as this now only
            -- works for linear paths..

            earthPrevious = updateParams.msgEarthPrevious
            earthNext = updateParams.msgEarth

            weight = ((updateParams.elapsed - updateParams.msgElapsedPrevious) / 
                      (updateParams.msgElapsed - updateParams.msgElapsedPrevious))

            weightedAve p1 p2 w = 
              p1 + w * (p2 - p1)

            earth = model.earth
            newEarth = 
              { earth | rotationTheta = weightedAve earthPrevious.rotationTheta earthNext.rotationTheta weight,
                        locationX = weightedAve earthPrevious.locationX earthNext.locationX weight,
                        locationY = weightedAve earthPrevious.locationY earthNext.locationY weight,
                        locationZ = weightedAve earthPrevious.locationZ earthNext.locationZ weight
              }

            -- Adjust power if controls up or down

            newPowerChange = (if model.controller.upButtonDown then 0.0001
                              else (if model.controller.downButtonDown then -0.0001 else 0))

            newPower = max 0 (min 2 (model.hero.power + (timeInBetween*newPowerChange)))

            -- Adjust altitude according to power

            newAltitudeChange = (newPower - 1) / 200
            newAltitude = max 10.5 (min 100 (model.hero.altitude + 
                                             (timeInBetween*newAltitudeChange)))

            -- Temporarily move hero around the world

            newLongitude = model.hero.longitude - timeInBetween * 0.00005

            -- Wiggle the balloon a bit

            newRotationTheta = sin (model.updateParams.elapsed / 1000) / 20

            -- Update hero params

            hero = model.hero
            newHero = { hero | rotationTheta = newRotationTheta, 
                               longitude = newLongitude,
                               power = newPower,
                               altitude = newAltitude } 


        in
          -- Aand update the final model.
          { model | hero = newHero,
                    updateParams = newUpdateParams,
                    earth = newEarth
          } 
      , Cmd.none 
      )

    -- Here mouse and touch related events are handled

    PointerEventMsg event -> 
      case event of 
        MouseUp struct ->
          let controller = 
                model.controller
              newController = 
                { controller | upButtonDown = False,
                               downButtonDown = False,
                               dragState = NoDrag }
          in 
            ( { model | controller = newController }, Cmd.none)

        MouseDown struct ->
          let offsetPos = 
                struct.offsetPos
              coordsInUp = 
                coordinatesWithinUpButton model offsetPos
              coordsInDown = 
                coordinatesWithinDownButton model offsetPos
              upButtonDown = 
                if coordsInUp then True else False
              downButtonDown = 
                if coordsInDown then True else False
              controller = 
                model.controller
              newController = 
                { controller | previousOffset = { x = round (Tuple.first offsetPos),
                                                  y = round (Tuple.second offsetPos)},
                               pointerOffset = { x = round (Tuple.first offsetPos),
                                                 y = round (Tuple.second offsetPos)},
                               upButtonDown = upButtonDown,
                               downButtonDown = downButtonDown,
                               dragState = Drag }
          in
            ( { model | controller = newController }, Cmd.none )

        MouseMove struct ->
          let offsetPos = struct.offsetPos
              newAzimoth = 
                (if model.controller.dragState == Drag 
                 then model.camera.azimoth - (toFloat (round (Tuple.first offsetPos) - 
                                                       model.controller.previousOffset.x)) * pi / 180
                 else model.camera.azimoth)
              newElevation = 
                (if model.controller.dragState == Drag 
                 then model.camera.elevation + (toFloat (round (Tuple.second offsetPos) - 
                                                         model.controller.previousOffset.y)) * pi / 180
                 else model.camera.elevation)
              camera = 
                model.camera
              newCamera = 
                { camera | azimoth = newAzimoth,
                           elevation = ( if newElevation <= (4*pi/10) 
                                         then ( if newElevation >= (-4*pi/10) 
                                                then newElevation else model.camera.elevation)
                                         else model.camera.elevation ) }
              controller = 
                model.controller
              newController = 
                { controller | previousOffset = { x = round (Tuple.first offsetPos),
                                                  y = round (Tuple.second offsetPos) } }

          in ( { model | camera = newCamera,
                         controller = newController
               }, Cmd.none )

        TouchUp struct ->
          let controller = 
                model.controller
              newController = 
                { controller | upButtonDown = False,
                               downButtonDown = False,
                               dragState = NoDrag }
          in 
            ( { model | controller = newController }, Cmd.none)

        TouchDown struct ->
          case (List.head struct.touches) of 
            Nothing -> (model, Cmd.none)
            Just x -> 
              let offsetPos = 
                    x.clientPos
                  coordsInUp = 
                    coordinatesWithinUpButton model offsetPos
                  coordsInDown = 
                    coordinatesWithinDownButton model offsetPos
                  upButtonDown = 
                    if coordsInUp then True else False
                  downButtonDown = 
                    if coordsInDown then True else False
                  controller = 
                    model.controller
                  newController = 
                    { controller | previousOffset = { x = round (Tuple.first offsetPos),
                                                      y = round (Tuple.second offsetPos)},
                                   pointerOffset = { x = round (Tuple.first offsetPos),
                                                     y = round (Tuple.second offsetPos)},
                                   upButtonDown = upButtonDown,
                                   downButtonDown = downButtonDown,
                                   dragState = Drag }
              in
                ( { model | controller = newController }, Cmd.none )

        TouchMove struct ->
          case (List.head struct.touches) of 
            Nothing -> (model, Cmd.none)
            Just x ->
              let offsetPos = 
                    x.clientPos
                  newAzimoth = 
                    (if model.controller.dragState == Drag 
                     then model.camera.azimoth - (toFloat (round (Tuple.first offsetPos) - 
                                                           model.controller.previousOffset.x)) * pi / 180
                     else model.camera.azimoth)
                  newElevation = 
                    (if model.controller.dragState == Drag 
                     then model.camera.elevation + (toFloat (round (Tuple.second offsetPos) - 
                                                             model.controller.previousOffset.y)) * pi / 180
                     else model.camera.elevation)
                  camera = 
                    model.camera
                  newCamera = 
                     { camera | azimoth = newAzimoth,
                                elevation =
                                  ( if newElevation <= (4*pi/10) 
                                    then (if newElevation >= (-4*pi/10) 
                                          then newElevation else model.camera.elevation)
                                    else model.camera.elevation ) }
                  controller =
                    model.controller
                  newController = 
                    { controller | previousOffset = { x = round (Tuple.first offsetPos),
                                                      y = round (Tuple.second offsetPos) } }

              in ( { model | camera = newCamera,
                             controller = newController
                   }, Cmd.none )

    -- Viewport related handlers.

    ResizeMsg -> 
      (model, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") ) 

    -- This Msg is often induced by code also, as it refreshes the screen..

    ViewportMsg returnValue -> 
      case returnValue of
        Ok struct ->
          ({ model | canvasDimensions = { width = round struct.viewport.width,
                                          height = round struct.viewport.height } }, 
           Cmd.none)
        Err errMsg -> (model, Cmd.none)

    -- Load meshes

    EarthMeshLoaded result -> 
      let oldEarth = model.earth 
          earth = { oldEarth | mesh = Result.toMaybe result }
         
      in ( { model | earth = earth }

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


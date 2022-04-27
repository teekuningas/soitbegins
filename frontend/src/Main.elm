module Main exposing (main)

import World exposing (heroMesh, fireMesh,
                       heroUnif, fireUnif,
                       earthMesh, earthUnif,
                       sunMesh, sunUnif)

import Receiver exposing (messageReceiver, decodeJson, RecvValue)

import Controller exposing (controllerMeshUp, controllerMeshDown, controllerUnif, 
                            coordinatesWithinUpButton, coordinatesWithinDownButton)

import Common exposing (Model, GameState(..), ConnectionState(..), DragState(..),
                        viewportSize, vertexShader, fragmentShader)

import Math.Vector3 as Vec3 exposing (vec3)

import Task
import Time
import List

import Browser
import Browser.Dom exposing (getViewportOf, Viewport)
import Browser.Events exposing (onAnimationFrame, onResize)

import Platform.Sub
import Platform.Cmd

import Html exposing (Html, div, text, button, p)
import Html.Attributes exposing (height, style, width, id, class)

import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch

import WebGL


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
  | RecvMsg RecvValue
  | RecvMsgError String
  | UpdateTimeMsg Time.Posix
  | StartGameMsg


-- The model initialization

init : () -> (Model, Cmd Msg)
init model = 
  let earth = { locationX = 100
              , locationY = 100
              , locationZ = 100
              , rotationTheta = 0 }
  in

  ( { hero = { height = 1.01
             , latitude = 0.5
             , longitude = 0
             , rotationTheta = 0
             , power = 1 } 
    , earth = earth
    , camera = { azimoth = 0
               , elevation = 0 }
    , updateParams = { msgElapsed = 0
                     , msgElapsedPrevious = 0
                     , msgEarth = earth
                     , msgEarthPrevious = earth
                     , elapsed = 0
                     , elapsedPrevious = 0 }
    , canvasDimensions = { width = 0, height = 0 }
    , controller = { dragState = NoDrag
                   , pointerOffset = { x = 0, y = 0 }
                   , previousOffset = { x = 0, y = 0 }
                   , upButtonDown = False
                   , downButtonDown = False } 
    , messages = []
    , gameState = MainMenu
    , connectionState = Disconnected
    }
  , Task.attempt ViewportMsg (getViewportOf "webgl-canvas") ) 


-- The view function

view : Model -> Html Msg
view model =
  let
    upButtonDown = model.controller.upButtonDown
    downButtonDown = model.controller.downButtonDown
    connectionState = model.connectionState
    gameState = model.gameState
  in
  case (gameState, connectionState) of 
    (MainMenu, _) -> 
      div [ class "main-menu-container" ] 
          [ p [] [ text "So it begins (the grand hot air balloon adventure)" ]
          , button [ onClick StartGameMsg ] [ text "Start here" ] ]
    (FlightMode, Disconnected) -> 
      div [] [text (Maybe.withDefault "Starting.." (List.head model.messages)) ]
    (FlightMode, Connected) ->
      div [] 
        [ div [] [ WebGL.toHtml [ 
                     width (Tuple.first viewportSize)
                   , height (Tuple.second viewportSize)
                   , style "display" "block"
                   , style "height" "90vh"
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
                   ]
                 ]
        ]

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = 
  Platform.Sub.batch 
    [ (onAnimationFrame (\x -> TimeElapsed x))
    , (onResize (\width height -> ResizeMsg))
    , (messageReceiver recvJson) ]


-- Updates

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    StartGameMsg ->
      ( { model | gameState = FlightMode }, 
          Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

    RecvMsgError message ->
      ( { model | messages = [message] ++ model.messages,
                  connectionState = Disconnected }, Cmd.none )

    -- Receive World parameters through a port.
    -- Store parameters, but do not touch the real thing yet.

    RecvMsg message ->
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

    -- This is called for each animation frame update. Here we construct a interpolated world
    -- which is reflected in the visuals

    TimeElapsed dt ->
      ( let 
            -- Update hero params

            timeInBetween = model.updateParams.elapsed - model.updateParams.elapsedPrevious

            newPowerChange = (if model.controller.upButtonDown then 0.001
                              else (if model.controller.downButtonDown then -0.001 else 0))

            newPower = max 0 (min 2 (model.hero.power + (timeInBetween*newPowerChange)))

            newHeightChange = (newPower - 1) * timeInBetween / 20000
            newHeight = max 1 (min 10 (model.hero.height + (timeInBetween*newHeightChange)))

            hero = model.hero
            newHero = { hero | rotationTheta = sin (model.updateParams.elapsed / 1000) / 10, 
                               power = newPower,
                               height = newHeight } 

            -- Store time related params

            updateParams = model.updateParams
            newUpdateParams = { updateParams | elapsed = toFloat (Time.posixToMillis dt),
                                               elapsedPrevious = updateParams.elapsed }

            -- Interpolate between two received earth messages

            earthPrevious = updateParams.msgEarthPrevious
            earthNext = updateParams.msgEarth

            weight = ((updateParams.elapsed - updateParams.msgElapsed) / 
                      (updateParams.msgElapsed - updateParams.msgElapsedPrevious))

            weightedAve p1 p2 w = 
              p1 + w * (p2 - p1)

            earth = model.earth
            newEarth = { earth | rotationTheta = weightedAve earthPrevious.rotationTheta earthNext.rotationTheta weight,
                                 locationX = weightedAve earthPrevious.locationX earthNext.locationX weight,
                                 locationY = weightedAve earthPrevious.locationY earthNext.locationY weight,
                                 locationZ = weightedAve earthPrevious.locationZ earthNext.locationZ weight
                       }

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
                           elevation = ( if newElevation <= (pi/3) 
                                         then ( if newElevation >= (-pi/3) 
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
                                  ( if newElevation <= (pi/3) 
                                    then (if newElevation >= (-pi/3) 
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

    -- Viewport related handlers

    ResizeMsg -> 
      (model, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") ) 

    ViewportMsg returnValue -> 
      case returnValue of
        Ok struct ->
          ({ model | canvasDimensions = { width = round struct.viewport.width,
                                          height = round struct.viewport.height } }, 
           Cmd.none)
        Err errMsg -> (model, Cmd.none)


-- Here it begins.


main : Program () Model Msg
main =
  Browser.element { init = init
                  , view = view
                  , subscriptions = subscriptions
                  , update = update }


-- Some helpers


recvJson : String -> Msg
recvJson value =
  case decodeJson value of
    Ok result ->
      RecvMsg result
    Err errorMessage ->
      RecvMsgError ("Error while communicating with the server")


fixOffset : { x : Int , y: Int } -> { width: Int, height: Int } -> { x : Int, y : Int }
fixOffset offset viewport = { x = round (toFloat (offset.x * (Tuple.first viewportSize)) / 
                                         (toFloat viewport.width)),
                              y = round (toFloat (offset.y * (Tuple.second viewportSize)) / 
                                         (toFloat viewport.height)) }


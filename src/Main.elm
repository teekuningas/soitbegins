module Main exposing (main)

import World exposing (heroMesh, fireMesh,
                       heroUnif, fireUnif)

import Controller exposing (controllerMeshUp, controllerMeshDown, controllerUnif, 
                            coordinatesWithinUpButton, coordinatesWithinDownButton)

import Common exposing (Model, DragState(..),
                        viewportSize, vertexShader, fragmentShader)

import Task

import Browser
import Browser.Dom exposing (getViewportOf, Viewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)

import Platform.Sub exposing (batch)

import Html exposing (Html, div, text)
import Html.Attributes exposing (height, style, width, id)

import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch

import WebGL


type PointerEvent = 
    MouseUp Mouse.Event
  | MouseDown Mouse.Event
  | MouseMove Mouse.Event
  | TouchMove Touch.Event
  | TouchDown Touch.Event
  | TouchUp Touch.Event


type Msg = TimeDelta Float
  | ResizeMsg
  | PointerEventMsg PointerEvent 
  | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)


init : () -> (Model, Cmd Msg)
init model = 
  ( { location = {x = 0, y = 0, z = 0} 
    , rotation = 0 
    , elapsed = 0
    , power = 1 
    , canvasDimensions = { width = 0, height = 0 }
    , dragState = NoDrag 
    , pointerOffset = { x = 0, y = 0 }
    , previousOffset = {x = 0, y = 0}
    , upButtonDown = False
    , downButtonDown = False
    , cameraAzimoth = 0 
    , cameraElevation = 0 }
  , Task.attempt ViewportMsg (getViewportOf "webgl-canvas") ) 


fixOffset : { x : Int , y: Int } -> { width: Int, height: Int } -> { x : Int, y : Int }
fixOffset offset viewport = { x = round (toFloat (offset.x * (Tuple.first viewportSize)) / 
                                         (toFloat viewport.width)),
                              y = round (toFloat (offset.y * (Tuple.second viewportSize)) / 
                                         (toFloat viewport.height)) }
       

view : Model -> Html Msg
view model =
  let
    upButtonDown = model.upButtonDown
    downButtonDown = model.downButtonDown
  in
  div [] 
    [ 
--    div [] [text ("Azimoth: " ++ (Debug.toString model.cameraAzimoth))]
--  , div [] [text ("Drag: " ++ (Debug.toString model.dragState))]
--  , div [] [text ("Elevation: " ++ (Debug.toString model.cameraElevation))]
--  , div [] [text ("Canvas dimensions: " ++ (Debug.toString model.canvasDimensions))]
--  , div [] [text ("Pointer offset: " ++ (Debug.toString model.pointerOffset))]
--  , div [] [ WebGL.toHtml [
      div [] [ WebGL.toHtml [ 
                   width (Tuple.first viewportSize)
                 , height (Tuple.second viewportSize)
                 , style "display" "block"
                 , style "height" "70vh"
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


subscriptions : Model -> Sub Msg
subscriptions _ = 
  batch [ (onAnimationFrameDelta (\x -> TimeDelta x))
        , (onResize (\width height -> ResizeMsg)) ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    TimeDelta dt ->
      ( let locationRec = model.location

            newPowerChange = (if model.upButtonDown then 0.01 
                              else (if model.downButtonDown then -0.01 else 0))

            newPower = max 0 (min 2 (model.power + newPowerChange))

        in
          { model | rotation = sin (model.elapsed / 1000) / 10, 
                    location = { locationRec | x = 0.5 * sin (model.elapsed / 1000) },
                    elapsed = model.elapsed + dt,
                    power = newPower
          } 
        , Cmd.none 
      )

    PointerEventMsg event -> 
      case event of 

        MouseUp struct ->
          ({ model | upButtonDown = False,
                     downButtonDown = False,
                     dragState = NoDrag }, Cmd.none)

        MouseDown struct ->
          let offsetPos = struct.offsetPos
              coordsInUp = coordinatesWithinUpButton model offsetPos
              coordsInDown = coordinatesWithinDownButton model offsetPos
              upButtonDown = if coordsInUp then True else False
              downButtonDown = if coordsInDown then True else False
          in
          ({ model | previousOffset = {x = round (Tuple.first offsetPos),
                                       y = round (Tuple.second offsetPos)},
                     pointerOffset = {x = round (Tuple.first offsetPos),
                                      y = round (Tuple.second offsetPos)},
                     upButtonDown = upButtonDown,
                     downButtonDown = downButtonDown,
                     dragState = Drag}, Cmd.none)

        MouseMove struct ->
          let offsetPos = struct.offsetPos
              newCameraAzimoth = 
                (if model.dragState == Drag 
                 then model.cameraAzimoth - (toFloat (round (Tuple.first offsetPos) - 
                                                      model.previousOffset.x)) * pi / 180
                 else model.cameraAzimoth)
              newCameraElevation = 
                (if model.dragState == Drag 
                 then model.cameraElevation + (toFloat (round (Tuple.second offsetPos) - 
                                                        model.previousOffset.y)) * pi / 180
                 else model.cameraElevation)
          in ({model | cameraAzimoth = newCameraAzimoth,
                       cameraElevation =
                         (if newCameraElevation <= (pi/3) 
                          then (if newCameraElevation >= (-pi/3) 
                                then newCameraElevation else model.cameraElevation)
                          else model.cameraElevation),
                       previousOffset = {x = round (Tuple.first offsetPos),
                                         y = round (Tuple.second offsetPos)} 
           }, Cmd.none)

        TouchUp struct ->
          ({ model | upButtonDown = False,
                     downButtonDown = False,
                     dragState = NoDrag }, Cmd.none)

        TouchDown struct ->
          case (List.head struct.touches) of 
            Nothing -> (model, Cmd.none)
            Just x -> let offsetPos = x.clientPos
                          coordsInUp = coordinatesWithinUpButton model offsetPos
                          coordsInDown = coordinatesWithinDownButton model offsetPos
                          upButtonDown = if coordsInUp then True else False
                          downButtonDown = if coordsInDown then True else False
                      in
                      ({ model | previousOffset = {x = round (Tuple.first offsetPos),
                                                   y = round (Tuple.second offsetPos)},
                                 pointerOffset = {x = round (Tuple.first offsetPos),
                                                  y = round (Tuple.second offsetPos)},
                                 upButtonDown = upButtonDown,
                                 downButtonDown = downButtonDown,
                                 dragState = Drag}, Cmd.none)

        TouchMove struct ->
          case (List.head struct.touches) of 
            Nothing -> (model, Cmd.none)
            Just x -> let offsetPos = x.clientPos
                          newCameraAzimoth = 
                            (if model.dragState == Drag 
                             then model.cameraAzimoth - (toFloat (round (Tuple.first offsetPos) - 
                                                                  model.previousOffset.x)) * pi / 180
                             else model.cameraAzimoth)
                          newCameraElevation = 
                            (if model.dragState == Drag 
                             then model.cameraElevation + (toFloat (round (Tuple.second offsetPos) - 
                                                                    model.previousOffset.y)) * pi / 180
                             else model.cameraElevation)

                      in ({model | cameraAzimoth = newCameraAzimoth,
                                   cameraElevation = 
                                     (if newCameraElevation <= (pi/3) 
                                      then (if newCameraElevation >= (-pi/3) 
                                            then newCameraElevation else model.cameraElevation)
                                      else model.cameraElevation),
                                   previousOffset = {x = round (Tuple.first offsetPos),
                                                     y = round (Tuple.second offsetPos)}}, Cmd.none)


    ResizeMsg -> 
      (model, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") ) 

    ViewportMsg returnValue -> 
      case returnValue of
        Ok struct ->
          ({ model | canvasDimensions = { width = round struct.viewport.width,
                                          height = round struct.viewport.height } }, 
           Cmd.none)
        Err errMsg -> (model, Cmd.none)


main : Program () Model Msg
main =
  Browser.element { init = init
                  , view = view
                  , subscriptions = subscriptions
                  , update = update }



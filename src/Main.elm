module Main exposing (main)

import World exposing (heroMesh, fireMesh,
                       heroUnif, fireUnif)

import Controller exposing (controllerMeshUp, controllerMeshDown, controllerUnif, 
                            coordinatesWithinUpButton, coordinatesWithinDownButton)

import Common exposing (Model, viewportSize, vertexShader, fragmentShader)

import Task

import Browser
import Browser.Dom exposing (getViewportOf, Viewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)

import Platform.Sub exposing (batch)

import Html exposing (Html, div, text)
import Html.Attributes exposing (height, style, width, id)

import Html.Events
import Html.Events.Extra.Pointer as Pointer

import WebGL


type PointerEvent = Down Pointer.Event
  | Move Pointer.Event
  | Up Pointer.Event
  | Cancel Pointer.Event


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
    , pointerOffset = { x = 0, y = 0 }
    , canvasDimensions = { width = 0, height = 0 }
    , upButtonDown = False
    , downButtonDown = False }
  , Task.attempt ViewportMsg (getViewportOf "webgl-canvas") ) 


fixOffset : { x : Int , y: Int } -> { width: Int, height: Int } -> { x : Int, y : Int }
fixOffset offset viewport = { x = round (toFloat (offset.x * (Tuple.first viewportSize)) / 
                                         (toFloat viewport.width)),
                              y = round (toFloat (offset.y * (Tuple.second viewportSize)) / 
                                         (toFloat viewport.height)) }
       

view : Model -> Html Msg
view model =
  div [] 
    [ div [] [text ("Canvas dimensions: " ++ (Debug.toString model.canvasDimensions))]
    , div [] [text ("Pointer offset: " ++ (Debug.toString model.pointerOffset))]
    , div [] [ WebGL.toHtml
                 [ width (Tuple.first viewportSize)
                 , height (Tuple.second viewportSize)
                 , style "display" "block"
                 , style "height" "70vh"
                 , style "width" "100vw"
                 , id "webgl-canvas"
                 , Pointer.onUp (PointerEventMsg << Up)
                 , Pointer.onDown (PointerEventMsg << Down) ]
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
                   (controllerUnif model (if model.upButtonDown then 1.0 else 0.5)))
                 , (WebGL.entity
                   vertexShader
                   fragmentShader
                   controllerMeshDown
                   (controllerUnif model (if model.downButtonDown then 1.0 else 0.5)))
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
        Up struct ->
          ({ model | pointerOffset = { x = round (Tuple.first struct.pointer.offsetPos),
                                       y = round (Tuple.second struct.pointer.offsetPos) },
                     upButtonDown = False,
                     downButtonDown = False }, Cmd.none)
        Down struct ->
          let coordsInUp = coordinatesWithinUpButton model struct.pointer.offsetPos
              coordsInDown = coordinatesWithinDownButton model struct.pointer.offsetPos
          in
          ({ model | pointerOffset = { x = round (Tuple.first struct.pointer.offsetPos),
                                       y = round (Tuple.second struct.pointer.offsetPos) },
                     upButtonDown = if coordsInUp then True else False,
                     downButtonDown = if coordsInDown then True else False }, Cmd.none)
        _ -> (model, Cmd.none)

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



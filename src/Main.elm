module Main exposing (main)

import World exposing (heroMesh, fireMesh, controllerMesh,
                       heroUnif, fireUnif, controllerUnif,
                       vertexShader, fragmentShader)

import Common exposing (Model, viewportSize)

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
    , fireStrength = 1 
    , pointerOffset = { x = 0, y = 0 }
    , canvasDimensions = { width = 0, height = 0 } }
  , Task.attempt ViewportMsg (getViewportOf "webgl-canvas") ) 


fixOffset : { x : Int , y: Int } -> { width: Int, height: Int } -> { x : Int, y : Int }
fixOffset offset viewport = { x = round (toFloat (offset.x * (Tuple.first viewportSize)) / 
                                         (toFloat viewport.width)),
                              y = round (toFloat (offset.y * (Tuple.second viewportSize)) / 
                                         (toFloat viewport.height)) }
       

view : Model -> Html Msg
view model =
  div [] 
    , div [] [text ("Canvas dimensions: " ++ (Debug.toString model.canvasDimensions))]
    , div [] [ WebGL.toHtml
                 [ width (Tuple.first viewportSize)
                 , height (Tuple.second viewportSize)
                 , style "display" "block"
                 , style "height" "80vh"
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
                   controllerMesh
                   (controllerUnif model))
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
        in
          { model | rotation = sin (model.elapsed / 1000) / 10, 
                    location = { locationRec | x = 0.5 * sin (model.elapsed / 1000) },
                    elapsed = model.elapsed + dt
          } 
        , Cmd.none 
      )

    PointerEventMsg event -> 
      case event of 
        Up struct ->
          ({ model | pointerOffset = { x = round (Tuple.first struct.pointer.offsetPos),
                                       y = round (Tuple.second struct.pointer.offsetPos) } }, Cmd.none)
        Down struct ->
          ({ model | pointerOffset = { x = round (Tuple.first struct.pointer.offsetPos),
                                       y = round (Tuple.second struct.pointer.offsetPos) } }, Cmd.none)
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


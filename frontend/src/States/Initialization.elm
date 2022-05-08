module States.Initialization exposing (view, update, subscriptions, init, Msg)

import Common exposing ( Model(..)
                       , Vertex
                       , InitData
                       )

import ObjLoader
import Obj.Decode exposing (expectObj)
import Platform.Cmd

import Length exposing (meters, Meters)

import Flags exposing (FlagsValue)

import Common exposing (viewportSize)

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onResize)

import Task

import WebGL exposing (Mesh)

import Page exposing ( embedInCanvas)

import Html exposing ( Html
                     , text 
                     , p
                     , div
                     )
import Html.Attributes exposing ( class )
import Http

import Platform.Sub


type Msg 
    = ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | EarthMeshLoaded (Result Http.Error (Mesh Vertex))


init : FlagsValue -> ( Model, Cmd Msg )
init flags =
            let
                initData =
                    { canvasDimensions =
                        { width = Tuple.first viewportSize
                        , height = Tuple.second viewportSize
                        }
                    }

                modelEarthUrl =
                    flags.modelEarth

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
                        ]
            in
            ( Initialization initData
            , cmd
            )



view : InitData -> Html Msg
view data = 
    embedInCanvas
      [ div
          [ class "initialization-container" ]
          [ p [] [ text "Loading assets.." ] ]
      ] [] []

update : Msg -> InitData -> ( Model, Cmd Msg )
update msg initData =

    case msg of 

        ResizeMsg ->
            ( Initialization initData, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )
    
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
                    |> Result.withDefault initData.canvasDimensions
    
                newInitData =
                    { initData | canvasDimensions = newCanvasDimensions }
            in
            ( Initialization newInitData
            , Cmd.none
            )
     

        EarthMeshLoaded result ->
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

subscriptions : InitData -> Sub Msg
subscriptions initData =
    Platform.Sub.batch
        [ onResize (\width height -> ResizeMsg)
        ]


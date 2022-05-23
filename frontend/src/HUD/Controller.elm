module HUD.Controller exposing
    ( controllerMeshDown
    , controllerMeshUp
    , controllerUnif
    , fragmentShader
    , handleDown
    , handleMove
    , handleUp
    , vertexShader
    )

import HUD.Page exposing (viewportSize)
import HUD.Types exposing (CanvasDimensions)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)
import World.Types exposing (Camera, Controller, DragState(..), MeshList, Uniforms, Vertex)


controllerParams : { x : Float, y : Float, size : Float, trans : Float }
controllerParams =
    { x = 0.5
    , y = -0.2
    , size = 0.2
    , trans = 0.3
    }


controllerUnif : CanvasDimensions -> Float -> Uniforms
controllerUnif canvasDim shade =
    let
        xscale =
            toFloat canvasDim.width
                / toFloat (Tuple.first viewportSize)

        yscale =
            toFloat canvasDim.height
                / toFloat (Tuple.second viewportSize)

        x =
            controllerParams.x

        y =
            controllerParams.y

        size =
            controllerParams.size

        translation =
            Mat4.translate (vec3 x y 0) Mat4.identity

        scale =
            Mat4.scale (vec3 (size / xscale) (size / yscale) 1) Mat4.identity
    in
    { preScale = Mat4.identity
    , preRotation = Mat4.identity
    , preTranslation = Mat4.identity
    , scale = scale
    , rotation = Mat4.identity
    , translation = translation
    , postScale = Mat4.identity
    , postRotation = Mat4.identity
    , postTranslation = Mat4.identity
    , perspective =
        Mat4.makeOrtho -1 1 -1 1 0 10
    , camera =
        Mat4.makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
    , shade = shade
    }


controllerMeshDown : Mesh Vertex
controllerMeshDown =
    let
        trans =
            controllerParams.trans
    in
    [ meshPositionMap
        (Vec3.add (vec3 0 -trans 0))
        [ ( Vertex (vec3 0 0 1) (vec3 1 0 0)
          , Vertex (vec3 1 0 0) (vec3 0 -1 0)
          , Vertex (vec3 0 0 1) (vec3 -1 0 0)
          )
        ]
    ]
        |> List.concat
        |> WebGL.triangles


controllerMeshUp : Mesh Vertex
controllerMeshUp =
    let
        trans =
            controllerParams.trans
    in
    [ meshPositionMap
        (Vec3.add (vec3 0 trans 0))
        [ ( Vertex (vec3 0 0 1) (vec3 -1 0 0)
          , Vertex (vec3 0 1 0) (vec3 0 1 0)
          , Vertex (vec3 0 0 1) (vec3 1 0 0)
          )
        ]
    ]
        |> List.concat
        |> WebGL.triangles


coordinatesWithinUpButton : CanvasDimensions -> ( Float, Float ) -> Bool
coordinatesWithinUpButton canvasDim offset =
    coordinatesWithinButton canvasDim offset (controllerParams.trans + 0.5)


coordinatesWithinDownButton : CanvasDimensions -> ( Float, Float ) -> Bool
coordinatesWithinDownButton canvasDim offset =
    coordinatesWithinButton canvasDim offset (-controllerParams.trans - 0.5)


coordinatesWithinButton : CanvasDimensions -> ( Float, Float ) -> Float -> Bool
coordinatesWithinButton canvasDim pointerOffset trans =
    let
        yscale =
            toFloat canvasDim.height
                / toFloat (Tuple.second viewportSize)

        xscale =
            toFloat canvasDim.width
                / toFloat (Tuple.first viewportSize)

        size =
            controllerParams.size

        fixedTrans =
            (trans * size) / yscale

        middlepointX =
            (1 + controllerParams.x) * (toFloat canvasDim.width / 2)

        middlepointY =
            (1 - controllerParams.y - fixedTrans) * (toFloat canvasDim.height / 2)

        sizeLimitX =
            size * toFloat (Tuple.first viewportSize) / 2

        sizeLimitY =
            size * toFloat (Tuple.second viewportSize) / 4
    in
    if
        (abs (middlepointX - Tuple.first pointerOffset) < sizeLimitX)
            && (abs (middlepointY - Tuple.second pointerOffset) < sizeLimitY)
    then
        True

    else
        False


handleUp : Controller -> Controller
handleUp controller =
    { controller
        | upButtonDown = False
        , downButtonDown = False
        , dragState = NoDrag
    }


handleDown : Controller -> ( Float, Float ) -> CanvasDimensions -> Controller
handleDown controller offsetPos canvasDim =
    let
        coordsInUp =
            coordinatesWithinUpButton canvasDim offsetPos

        coordsInDown =
            coordinatesWithinDownButton canvasDim offsetPos

        upButtonDown =
            if coordsInUp then
                True

            else
                False

        downButtonDown =
            if coordsInDown then
                True

            else
                False

        newController =
            { controller
                | previousOffset =
                    { x = round (Tuple.first offsetPos)
                    , y = round (Tuple.second offsetPos)
                    }
                , pointerOffset =
                    { x = round (Tuple.first offsetPos)
                    , y = round (Tuple.second offsetPos)
                    }
                , upButtonDown = upButtonDown
                , downButtonDown = downButtonDown
                , dragState = Drag
            }
    in
    newController


handleMove : Controller -> Camera -> ( Float, Float ) -> ( Controller, Camera )
handleMove controller camera offsetPos =
    let
        newAzimoth =
            if controller.dragState == Drag then
                camera.azimoth
                    - toFloat
                        (round (Tuple.first offsetPos)
                            - controller.previousOffset.x
                        )
                    * pi
                    / 180

            else
                camera.azimoth

        newElevation =
            if controller.dragState == Drag then
                camera.elevation
                    + toFloat
                        (round (Tuple.second offsetPos)
                            - controller.previousOffset.y
                        )
                    * pi
                    / 180

            else
                camera.elevation

        newCamera =
            { camera
                | azimoth = newAzimoth
                , elevation =
                    if newElevation <= (4 * pi / 10) then
                        if newElevation >= (-4 * pi / 10) then
                            newElevation

                        else
                            camera.elevation

                    else
                        camera.elevation
            }

        newController =
            { controller
                | previousOffset =
                    { x = round (Tuple.first offsetPos)
                    , y = round (Tuple.second offsetPos)
                    }
            }
    in
    ( newController, newCamera )


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
     attribute vec3 position;
     attribute vec3 color;
     uniform mat4 perspective;
     uniform mat4 camera;
     uniform mat4 preScale;
     uniform mat4 preRotation;
     uniform mat4 preTranslation;
     uniform mat4 scale;
     uniform mat4 rotation;
     uniform mat4 translation;
     uniform mat4 postScale;
     uniform mat4 postRotation;
     uniform mat4 postTranslation;
     varying vec3 vcolor;
     void main () {
       gl_Position = (perspective * camera * 
                      postTranslation * postRotation * postScale * 
                      translation * rotation * scale *
                      preTranslation * preRotation * preScale * vec4(position, 1.0));
       vcolor = color;
     }
  |]



-- Fragment shader


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
    precision mediump float;
    uniform float shade;
    varying vec3 vcolor;
    void main () {
      gl_FragColor = shade * vec4(vcolor, 1.0);
    }
  |]



-- A utility map for Vertex positions


meshPositionMap : (Vec3 -> Vec3) -> MeshList -> MeshList
meshPositionMap fun mesh =
    case mesh of
        [] ->
            []

        ( v1, v2, v3 ) :: xs ->
            ( { v1 | position = fun v1.position }
            , { v2 | position = fun v2.position }
            , { v3 | position = fun v3.position }
            )
                :: meshPositionMap fun xs

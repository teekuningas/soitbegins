module Controller exposing (controllerMeshUp, controllerMeshDown, controllerUnif, 
                            coordinatesWithinUpButton, coordinatesWithinDownButton)

import Common exposing (Model, viewportSize, meshPositionMap,
                        MeshList, Vertex, Uniforms)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)

import WebGL exposing (Mesh)


-- Some global controller related variables 

controllerParams : { x: Float, y: Float, size: Float, trans: Float }
controllerParams = { x = 0.5
                   , y = -0.2 
                   , size = 0.2
                   , trans = 0.3 }


-- Controller uniforms

controllerUnif : Model -> Float -> Uniforms
controllerUnif model shade =
  let xscale = ((toFloat model.canvasDimensions.width) /
                (toFloat (Tuple.first viewportSize)))
      yscale = ((toFloat model.canvasDimensions.height) /
                (toFloat (Tuple.second viewportSize)))
      x = controllerParams.x
      y = controllerParams.y
      size = controllerParams.size

      translation = Mat4.translate (vec3 x y 0) Mat4.identity
      scale = Mat4.scale (vec3 (size/xscale) (size/yscale) 1) Mat4.identity
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
  , shade = shade } 


-- Controller meshes

controllerMeshDown : Mesh Vertex
controllerMeshDown =
  let trans = controllerParams.trans
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
  let trans = controllerParams.trans
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


-- Helpers for deciding if touch / cursor location
-- is within a control

coordinatesWithinUpButton : Model -> (Float, Float) -> Bool
coordinatesWithinUpButton model offset = 
  coordinatesWithinButton model offset (controllerParams.trans + 0.5)

coordinatesWithinDownButton : Model -> (Float, Float) -> Bool
coordinatesWithinDownButton model offset = 
  coordinatesWithinButton model offset (-controllerParams.trans - 0.5)


coordinatesWithinButton : Model -> (Float, Float) -> Float -> Bool
coordinatesWithinButton model pointerOffset trans =
  let yscale = ((toFloat model.canvasDimensions.height) /
                (toFloat (Tuple.second viewportSize)))
      xscale = ((toFloat model.canvasDimensions.width) /
                (toFloat (Tuple.first viewportSize)))

      -- Uses a heuristic square-based approach to decide if 
      -- the cursor / touch is within a control

      size = controllerParams.size
      fixedTrans = (trans * size) / yscale

      middlepointX = 
        (1 + controllerParams.x) * (toFloat (model.canvasDimensions.width) / 2)
      middlepointY = 
        (1 - controllerParams.y - fixedTrans) * (toFloat (model.canvasDimensions.height) / 2)

      sizeLimitX = size * toFloat (Tuple.first viewportSize) / 2
      sizeLimitY = size * toFloat (Tuple.second viewportSize) / 4

  in
    if (((abs (middlepointX - (Tuple.first pointerOffset))) < sizeLimitX) &&
        ((abs (middlepointY - (Tuple.second pointerOffset))) < sizeLimitY)) 
    then True
    else False
    

module Controller exposing (controllerMesh, controllerUnif)

import Common exposing (Model, viewportSize, meshPositionMap,
                        MeshList, Vertex, Uniforms)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)

import WebGL exposing (Mesh)


controllerUnif : Model -> Float -> Float -> Float -> Uniforms
controllerUnif model x y size =
  let xscale = ((toFloat model.canvasDimensions.width) /
                (toFloat (Tuple.first viewportSize)))
      yscale = ((toFloat model.canvasDimensions.height) /
                (toFloat (Tuple.second viewportSize)))
  in
  { rotation = Mat4.identity

  , location = 
      Mat4.translate (vec3 x y 0) Mat4.identity

  , perspective = 
      Mat4.makeOrtho -1 1 -1 1 0 10

  , camera = 
      Mat4.makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)

  , scale =
      Mat4.scale (vec3 (size/xscale) (size/yscale) 1) Mat4.identity

  , shade = 0.5 } 


controllerMesh : Mesh Vertex
controllerMesh =
  [ meshPositionMap 
    (Vec3.add (vec3 0 0.3 0))
    [ ( Vertex (vec3 0 0 1) (vec3 -1 0 0)
      , Vertex (vec3 0 1 0) (vec3 0 1 0)
      , Vertex (vec3 0 0 1) (vec3 1 0 0)
      )
    ]
  , meshPositionMap 
    (Vec3.add (vec3 0 -0.3 0))
    [ ( Vertex (vec3 0 0 1) (vec3 1 0 0)
      , Vertex (vec3 1 0 0) (vec3 0 -1 0)
      , Vertex (vec3 0 0 1) (vec3 -1 0 0)
      )
    ]

  ]
  |> List.concat
  |> WebGL.triangles



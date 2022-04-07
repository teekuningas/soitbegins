module World exposing (heroMesh, fireMesh, 
                       heroUnif, fireUnif)

import Common exposing (Model, viewportSize, meshPositionMap,
                        MeshList, Vertex, Uniforms)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)

import WebGL exposing (Mesh)


type alias Uniforms = 
  { rotation : Mat4
  , location : Mat4
  , perspective : Mat4
  , camera : Mat4
  , scale : Mat4
  , shade : Float
  }


type alias Vertex =
  { color : Vec3
  , position : Vec3 
  }


type alias MeshList = List (Vertex, Vertex, Vertex)


makeCamera : Float -> Float -> Mat4
makeCamera azimoth elevation =
  let locStart = (vec3 0 0 15)
      locAz = Mat4.transform (Mat4.makeRotate azimoth (vec3 0 1 0)) locStart
      locElv = Mat4.transform (Mat4.makeRotate elevation 
                               (Vec3.cross locAz (vec3 0 1 0))) locAz
      up = vec3 0 1 0
  in
    Mat4.makeLookAt locElv (vec3 0 2 0) up


worldUnif : Model -> Uniforms
worldUnif model =
  let aspect = ((toFloat model.canvasDimensions.width) / 
                (toFloat model.canvasDimensions.height))
  in
  { rotation = 
      Mat4.mul (Mat4.makeRotate (3 * model.rotation) (vec3 0 1 0))
               (Mat4.makeRotate (2 * model.rotation) (vec3 1 0 0))
  , location = 
      Mat4.translate (vec3 
                      model.location.x 
                      model.location.y 
                      model.location.z) Mat4.identity

  , perspective = 
      Mat4.makePerspective 45 aspect 0.01 100

  , camera = 
      makeCamera model.cameraAzimoth model.cameraElevation

  , scale =
      Mat4.scale (vec3 1 1 1) Mat4.identity

  , shade = 0.75 } 


heroUnif : Model -> Uniforms
heroUnif model = worldUnif model



fireUnif : Model -> Uniforms
fireUnif model = 
  let unif = heroUnif model
  in
    { unif | scale = (Mat4.scale 
                      (vec3 (model.power / 2) (model.power / 2) (model.power / 2)) 
                      Mat4.identity),
             location = Mat4.mul unif.rotation (Mat4.mul 
                                                unif.location 
                                                (Mat4.translate 
                                                 (vec3 0 1.75 0) 
                                                 Mat4.identity)) }


heroMesh : Mesh Vertex
heroMesh = 
  let balloonColor = Vec3.scale (1/ 255) (vec3 115 210 22) -- green
  in 
    [ cubeMeshList
    , meshPositionMap 
       (Vec3.add (vec3 0 4 0)) 
       (meshPositionMap (Vec3.scale 2) (sphereMeshList balloonColor))
    ]
    |> List.concat
    |> WebGL.triangles


fireMesh : Mesh Vertex
fireMesh = 
  let fireColor = Vec3.scale (1/ 255) (vec3 245 121 0) -- orange
  in 
    [ sphereMeshList fireColor
    ]
    |> List.concat
    |> WebGL.triangles


sphereMeshList : Vec3 -> MeshList
sphereMeshList clr =
  let phi = (1.0 + sqrt 5.0) * 0.5
      a = 1.0
      b = 1.0 / phi
      v1 = Vertex clr (Vec3.normalize (vec3 0 b -a))
      v2 = Vertex clr (Vec3.normalize (vec3 b a 0))
      v3 = Vertex clr (Vec3.normalize (vec3 -b a 0))
      v4 = Vertex clr (Vec3.normalize (vec3 0 b a))
      v5 = Vertex clr (Vec3.normalize (vec3 0 -b a))
      v6 = Vertex clr (Vec3.normalize (vec3 -a 0 b))
      v7 = Vertex clr (Vec3.normalize (vec3 0 -b a))
      v8 = Vertex clr (Vec3.normalize (vec3 a 0 -b))
      v9 = Vertex clr (Vec3.normalize (vec3 a 0 b))
      v10 = Vertex clr (Vec3.normalize (vec3 -a 0 -b))
      v11 = Vertex clr (Vec3.normalize (vec3 b -a 0))
      v12 = Vertex clr (Vec3.normalize (vec3 -b -a 0))
  in 
    [ (v3, v2, v1)
    , (v2, v3, v4)
    , (v6, v5, v4)
    , (v5, v9, v4)
    , (v8, v7, v1)
    , (v7, v10, v1)
    , (v12, v11, v5)
    , (v11, v12, v7)
    , (v10, v6, v3)
    , (v6, v10, v12)
    , (v9, v8, v2)
    , (v8, v9, v11)
    , (v3, v6, v4)
    , (v9, v2, v4)
    , (v10, v3, v1)
    , (v2, v8, v1)
    , (v12, v10, v7)
    , (v8, v11, v7)
    , (v6, v12, v5)
    , (v11, v9, v5) ]


cubeMeshList : MeshList
cubeMeshList =
  let
    rft =
      vec3 1 1 1
    lft =
      vec3 -1 1 1
    lbt =
      vec3 -1 -1 1
    rbt =
      vec3 1 -1 1
    rbb =
      vec3 1 -1 -1
    rfb =
      vec3 1 1 -1
    lfb =
      vec3 -1 1 -1
    lbb =
      vec3 -1 -1 -1
  in
    [ face (vec3 115 210 22) rft rfb rbb rbt -- green
    , face (vec3 52 101 164) rft rfb lfb lft -- blue
    , face (vec3 237 212 0) rft lft lbt rbt -- yellow
    , face (vec3 204 0 0) rfb lfb lbb rbb -- red
    , face (vec3 117 80 123) lft lfb lbb lbt -- purple
    , face (vec3 245 121 0) rbt rbb lbb lbt -- orange
    ]
    |> List.concat


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> MeshList
face color a b c d =
  let
    vertex position =
      Vertex (Vec3.scale (1 / 255) color) position
  in
    [ ( vertex a, vertex b, vertex c )
    , ( vertex c, vertex d, vertex a )
    ]


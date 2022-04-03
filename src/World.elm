module World exposing (heroMesh, fireMesh, controllerMesh, 
                       heroUnif, controllerUnif, fireUnif,
                       vertexShader, fragmentShader)

import Common exposing (Model)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)

import WebGL exposing (Mesh, Shader)


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


heroUnif : Model -> Uniforms
heroUnif model =
  { rotation = 
      Mat4.mul (Mat4.makeRotate (3 * model.rotation) (vec3 0 1 0))
               (Mat4.makeRotate (2 * model.rotation) (vec3 1 0 0))
  , location = 
      Mat4.translate (vec3 
                      model.location.x 
                      model.location.y 
                      model.location.z) Mat4.identity

  , perspective = 
      Mat4.makePerspective 45 1.5 0.01 100

  , camera = 
      Mat4.makeLookAt (vec3 0 0 15) (vec3 0 0 0) (vec3 0 1 0)

  , scale =
      Mat4.scale (vec3 1 1 1) Mat4.identity

  , shade = 0.5 } 


fireUnif : Model -> Uniforms
fireUnif model = 
  let unif = heroUnif model
  in
    { unif | scale = (Mat4.scale 
                      (vec3 0.5 0.5 0.5) 
                      Mat4.identity),
             location = Mat4.mul unif.rotation (Mat4.mul 
                                                unif.location 
                                                (Mat4.translate 
                                                 (vec3 0 1.75 0) 
                                                 Mat4.identity)) }


controllerUnif : Model -> Uniforms
controllerUnif model =
  { rotation = Mat4.identity

  , location = 
      Mat4.translate (vec3 2 -1 0) Mat4.identity

  , perspective = 
      Mat4.makePerspective 45 1.5 0.01 100

  , camera = 
      Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)

  , scale =
      Mat4.scale (vec3 0.5 0.5 0.5) Mat4.identity

  , shade = 0.5 } 


meshPositionMap : (Vec3 -> Vec3) -> MeshList -> MeshList
meshPositionMap fun mesh =
  case mesh of 
    [] -> 
      []
    (v1, v2, v3) :: xs ->
      [ ( { v1 | position = fun v1.position }
        , { v2 | position = fun v2.position }
        , { v3 | position = fun v3.position } ) ] ++ (meshPositionMap fun xs)
    

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


controllerMesh : Mesh Vertex
controllerMesh =
  [ meshPositionMap 
    (Vec3.add (vec3 0 0.2 0))
    [ ( Vertex (vec3 0 0 1) (vec3 -1 0 0)
      , Vertex (vec3 0 1 0) (vec3 0 1 0)
      , Vertex (vec3 0 0 1) (vec3 1 0 0)
      )
    ]
  , meshPositionMap 
    (Vec3.add (vec3 0 -0.2 0))
    [ ( Vertex (vec3 0 0 1) (vec3 1 0 0)
      , Vertex (vec3 1 0 0) (vec3 0 -1 0)
      , Vertex (vec3 0 0 1) (vec3 -1 0 0)
      )
    ]

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


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
  [glsl|
     attribute vec3 position;
     attribute vec3 color;
     uniform mat4 perspective;
     uniform mat4 camera;
     uniform mat4 rotation;
     uniform mat4 location;
     uniform mat4 scale;
     varying vec3 vcolor;
     void main () {
       gl_Position = (perspective * camera * location * 
                      rotation * scale * vec4(position, 1.0));
       vcolor = color;
     }
  |]


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


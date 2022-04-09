module World exposing (heroMesh, fireMesh, 
                       heroUnif, fireUnif)

import Common exposing (Model, viewportSize, meshPositionMap,
                        MeshList, Vertex, Uniforms)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)

import WebGL exposing (Mesh)


makeCamera : Float -> Float -> Mat4
makeCamera azimoth elevation =
  let locStart = (vec3 0 0 15)
      locAz = Mat4.transform (Mat4.makeRotate azimoth (vec3 0 1 0)) locStart
      locElv = Mat4.transform (Mat4.makeRotate elevation 
                               (Vec3.cross locAz (vec3 0 1 0))) locAz
      up = vec3 0 1 0
  in
    Mat4.makeLookAt locElv (vec3 0 2 0) up


generalUnif : Model -> Uniforms
generalUnif model =
  let aspect = ((toFloat model.canvasDimensions.width) / 
                (toFloat model.canvasDimensions.height))
  in
  { rotation = Mat4.identity 
  , location = Mat4.identity
  , scale = Mat4.identity
  , perspective = 
      Mat4.makePerspective 45 aspect 0.01 100
  , camera = 
      makeCamera model.camera.azimoth model.camera.elevation
  , shade = 0.75 } 


earthUnif : Model -> Uniforms
earthUnif model = 
  generalUnif model


heroUnif : Model -> Uniforms
heroUnif model =
  let unif = generalUnif model
  in
  { unif | rotation = Mat4.mul (Mat4.makeRotate (3 * model.hero.rotationTheta) (vec3 0 1 0))
                               (Mat4.makeRotate (2 * model.hero.rotationTheta) (vec3 1 0 0)),
           location = Mat4.translate (vec3 
                                      model.hero.locationX
                                      model.hero.locationY 
                                      model.hero.locationZ) Mat4.identity,
           scale = Mat4.scale (vec3 1 1 1) Mat4.identity } 


fireUnif : Model -> Uniforms
fireUnif model = 
  let unif = heroUnif model
  in
    { unif | scale = (Mat4.scale 
                      (vec3 (model.hero.power / 2) 
                            (model.hero.power / 2) 
                            (model.hero.power / 2)) 
                      Mat4.identity),
             location = Mat4.mul unif.rotation (Mat4.mul 
                                                unif.location 
                                                (Mat4.translate 
                                                 (vec3 0 1.6 0) 
                                                 Mat4.identity)) }


subdivideProject : MeshList -> MeshList
subdivideProject mesh =
  let helper m =
        case m of 
          [] -> 
            []
          (v1, v2, v3) :: xs ->
            let 
                mp12X = ((Vec3.getX v1.position) + 
                         (Vec3.getX v2.position)) / 2
                mp12Y = ((Vec3.getY v1.position) +
                         (Vec3.getY v2.position)) / 2
                mp12Z = ((Vec3.getZ v1.position) +
                         (Vec3.getZ v2.position)) / 2

                mp13X = ((Vec3.getX v1.position) + 
                         (Vec3.getX v3.position)) / 2
                mp13Y = ((Vec3.getY v1.position) +
                         (Vec3.getY v3.position)) / 2
                mp13Z = ((Vec3.getZ v1.position) +
                         (Vec3.getZ v3.position)) / 2

                mp23X = ((Vec3.getX v2.position) + 
                         (Vec3.getX v3.position)) / 2
                mp23Y = ((Vec3.getY v2.position) +
                         (Vec3.getY v3.position)) / 2
                mp23Z = ((Vec3.getZ v2.position) +
                         (Vec3.getZ v3.position)) / 2

                clr12R = ((Vec3.getX v1.color) +
                          (Vec3.getX v2.color)) / 2
                clr12G = ((Vec3.getY v1.color) +
                          (Vec3.getY v2.color)) / 2
                clr12B = ((Vec3.getZ v1.color) +
                          (Vec3.getZ v2.color)) / 2

                clr13R = ((Vec3.getX v1.color) +
                          (Vec3.getX v3.color)) / 2
                clr13G = ((Vec3.getY v1.color) +
                          (Vec3.getY v3.color)) / 2
                clr13B = ((Vec3.getZ v1.color) +
                          (Vec3.getZ v3.color)) / 2

                clr23R = ((Vec3.getX v2.color) +
                          (Vec3.getX v3.color)) / 2
                clr23G = ((Vec3.getY v2.color) +
                          (Vec3.getY v3.color)) / 2
                clr23B = ((Vec3.getZ v2.color) +
                          (Vec3.getZ v3.color)) / 2

                pos12 = vec3 mp12X mp12Y mp12Z
                pos13 = vec3 mp13X mp13Y mp13Z
                pos23 = vec3 mp23X mp23Y mp23Z

                clr12 = vec3 clr12R clr12G clr12B
                clr13 = vec3 clr13R clr13G clr13B
                clr23 = vec3 clr23R clr23G clr23B

                -- clr12 = Vec3.scale (1/ 255) (vec3 100 100 100)
                -- clr13 = Vec3.scale (1/ 255) (vec3 100 100 100)
                -- clr23 = Vec3.scale (1/ 255) (vec3 100 100 100)

                v12 = Vertex clr12 (Vec3.normalize pos12)
                v13 = Vertex clr13 (Vec3.normalize pos13)
                v23 = Vertex clr23 (Vec3.normalize pos23)
            in 
              [
               (v1, v12, v13),
               (v12, v2, v23),
               (v23, v3, v13),
               (v13, v12, v23)
              ] :: helper xs
  in List.concat (helper mesh)
                 

heroMesh : Mesh Vertex
heroMesh = 
  let balloonColor = Vec3.scale (1/ 255) (vec3 115 210 22) -- green
  in 
    [ cubeMeshList
    , meshPositionMap 
       (Vec3.add (vec3 0 4 0)) 
       (meshPositionMap 
        (Vec3.scale 2) 
        (subdivideProject (icosaMeshList balloonColor)))
    ]
    |> List.concat
    |> WebGL.triangles


fireMesh : Mesh Vertex
fireMesh = 
  let fireColor = Vec3.scale (1/ 255) (vec3 245 121 0) -- orange
  in 
    [ icosaMeshList fireColor
    ]
    |> List.concat
    |> WebGL.triangles


icosaMeshList : Vec3 -> MeshList
icosaMeshList clr =
  let phi = (1.0 + sqrt 5.0) * 0.5
      a = 1.0
      b = 1.0 / phi
--    v1 = Vertex (Vec3.add (vec3 0.01 0.01 0.01) clr) (Vec3.normalize (vec3 0 b -a))
--    v2 = Vertex (Vec3.add (vec3 0.03 0.03 0.03) clr) (Vec3.normalize (vec3 b a 0))
--    v3 = Vertex (Vec3.add (vec3 0.05 0.05 0.05) clr) (Vec3.normalize (vec3 -b a 0))
--    v4 = Vertex (Vec3.add (vec3 0.07 0.07 0.07) clr) (Vec3.normalize (vec3 0 b a))
--    v5 = Vertex (Vec3.add (vec3 0.09 0.09 0.09) clr) (Vec3.normalize (vec3 0 -b a))
--    v6 = Vertex (Vec3.add (vec3 0.11 0.11 0.11) clr) (Vec3.normalize (vec3 -a 0 b))
--    v7 = Vertex (Vec3.add (vec3 0.13 0.13 0.13) clr) (Vec3.normalize (vec3 0 -b -a))
--    v8 = Vertex (Vec3.add (vec3 0.15 0.15 0.15) clr) (Vec3.normalize (vec3 a 0 -b))
--    v9 = Vertex (Vec3.add (vec3 0.17 0.17 0.17) clr) (Vec3.normalize (vec3 a 0 b))
--    v10 = Vertex (Vec3.add (vec3 0.19 0.19 0.19) clr) (Vec3.normalize (vec3 -a 0 -b))
--    v11 = Vertex (Vec3.add (vec3 0.21 0.21 0.21) clr) (Vec3.normalize (vec3 b -a 0))
--    v12 = Vertex (Vec3.add (vec3 0.23 0.23 0.23) clr) (Vec3.normalize (vec3 -b -a 0))

      v1 = Vertex clr (Vec3.normalize (vec3 0 b -a))
      v2 = Vertex clr (Vec3.normalize (vec3 b a 0))
      v3 = Vertex clr (Vec3.normalize (vec3 -b a 0))
      v4 = Vertex clr (Vec3.normalize (vec3 0 b a))
      v5 = Vertex clr (Vec3.normalize (vec3 0 -b a))
      v6 = Vertex clr (Vec3.normalize (vec3 -a 0 b))
      v7 = Vertex clr (Vec3.normalize (vec3 0 -b -a))
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


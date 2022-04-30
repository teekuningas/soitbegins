module World exposing (heroMesh, heroUnif, 
                       fireMesh, fireUnif,
                       axisMesh, earthUnif,
                       sunMesh, sunUnif)

import Common exposing (Model, viewportSize, meshPositionMap,
                        MeshList, Vertex, Uniforms)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)

import WebGL exposing (Mesh)


-- A simple basis for uniforms,
-- which incldes the the correct perspective
-- and camera transformations

generalUnif : Model -> Uniforms
generalUnif model =
  let aspect = ((toFloat model.canvasDimensions.width) / 
                (toFloat model.canvasDimensions.height))
  in
  { preScale =
      Mat4.identity 
  , preRotation = 
      Mat4.identity
  , preTranslation =
      Mat4.identity
  , scale = 
      Mat4.identity
  , rotation = 
      Mat4.identity
  , translation =
      Mat4.identity
  , postScale = 
      Mat4.identity
  , postRotation =
      Mat4.identity
  , postTranslation =
      Mat4.identity
  , perspective = 
      Mat4.makePerspective 45 aspect 0.001 100000
  , camera = 
      makeHeroCamera model
  , shade = 0.75 } 


-- Uniforms for the sun

sunUnif : Model -> Uniforms
sunUnif model =
  let unif = generalUnif model
      -- Sun lies at the origin but is scaled
      scale = (Mat4.scale (vec3 2 2 2) Mat4.identity)
  in { unif | scale = scale}


-- Uniforms for the earth

earthUnif : Model -> Uniforms
earthUnif model = 
  let unif = generalUnif model

      scale = (Mat4.scale (vec3 10 10 10) Mat4.identity)

      -- Tilt and rotate along the correct axis
      rotation = (Mat4.mul 
                  (Mat4.makeRotate ((23.5/180)*pi) (vec3 0 0 1))
                  (Mat4.makeRotate model.earth.rotationTheta (vec3 0 1 0)))


      -- Move to the correct location
      translation = Mat4.translate (vec3 model.earth.locationX
                                         model.earth.locationY
                                         model.earth.locationZ) Mat4.identity


      
  in
  { unif | rotation = rotation,
           translation = translation }


-- Uniforms for the hero

heroUnif : Model -> Uniforms
heroUnif model =
  let unif = generalUnif model

      -- Hero size
      scale = (Mat4.scale (vec3 0.001 0.001 0.001) Mat4.identity)

      -- Hero wiggling
      rotation = Mat4.mul (Mat4.makeRotate (3 * model.hero.rotationTheta) (vec3 0 1 0))
                          (Mat4.makeRotate (2 * model.hero.rotationTheta) (vec3 1 0 0))

      -- Hero moved directly up from hero origin
      translation = (Mat4.translate (Vec3.scale model.hero.height Vec3.j) Mat4.identity)

      -- Hero rotated along different axis in the origin, effectively
      -- translating hero to correct position in a correct orientation
      earthAxis = (Mat4.transform 
                   (Mat4.makeRotate ((23.5/180)*pi) (vec3 0 0 1))
                   Vec3.j)
      latitudeAxis = Vec3.cross earthAxis (vec3 1 0 0)
      alignRotation = (Mat4.makeRotate ((23.5/180)*pi) (vec3 0 0 1))
      latitudeRotation = (Mat4.makeRotate (pi/2 - model.hero.latitude) latitudeAxis)
      longitudeRotation = (Mat4.makeRotate (model.hero.longitude) earthAxis)
      earthRotationRotation = (Mat4.makeRotate model.earth.rotationTheta earthAxis)
      postRotation = (List.foldl
                      Mat4.mul 
                      Mat4.identity [alignRotation,
                                     latitudeRotation,
                                     longitudeRotation,
                                     earthRotationRotation])

      -- And finally moved to the earth
      postTranslation = Mat4.translate (vec3 model.earth.locationX
                                             model.earth.locationY
                                             model.earth.locationZ) Mat4.identity

  in
  { unif | scale = scale,
           rotation = rotation,
           translation = translation,
           postRotation = postRotation,
           postTranslation = postTranslation }


-- Uniforms for the fire within hero.

fireUnif : Model -> Uniforms
fireUnif model = 
  let unif = heroUnif model
      -- The power source fire is approximately where the hero origin is
      -- but we move and scale it in the hero space to the exact
      -- correct location.
      preScale = (Mat4.scale (vec3 (model.hero.power / 2) 
                                   (model.hero.power / 2) 
                                   (model.hero.power / 2)) 
                             Mat4.identity)
      preTranslation = 
        Mat4.translate (vec3 0 1.6 0) Mat4.identity

  in
    { unif | preScale = preScale,
             preTranslation = preTranslation }
                

-- Constructs a simple mesh for earth

-- earthMesh : Mesh Vertex
-- earthMesh = 
--   let earthColor = Vec3.scale (1/255) (vec3 52 101 164) -- blue
--       divideColor = Vec3.scale (1/255) (vec3 115 210 22) -- green
--       axisColor = Vec3.scale (1/255) (vec3 204 0 0) -- red
--   in 
--    [
--    -- (subdivideProject divideColor (subdivideProject earthColor (icosaMeshList divideColor))),
--    (subdivideProject divideColor <| 
--     subdivideProject earthColor <| 
--     subdivideProject divideColor <| 
--     subdivideProject earthColor <| 
--     icosaMeshList divideColor),
--    (meshPositionMap (Vec3.add (vec3 0 1.5 0))
--     (meshPositionMap (Vec3.scale 0.1) (icosaMeshList axisColor))),
--    (meshPositionMap (Vec3.add (vec3 0 -1.5 0))
--     (meshPositionMap (Vec3.scale 0.1) (icosaMeshList axisColor))),
--    (meshPositionMap (Vec3.add (vec3 0 1.25 0))
--     (meshPositionMap (Vec3.scale 0.1) (icosaMeshList axisColor))),
--    (meshPositionMap (Vec3.add (vec3 0 -1.25 0))
--     (meshPositionMap (Vec3.scale 0.1) (icosaMeshList axisColor)))
--
--    ]
--    |> List.concat
--    |> WebGL.triangles

axisMesh : Mesh Vertex
axisMesh = 
 let axisColor = Vec3.scale (1/255) (vec3 204 0 0) -- red
   in 
    [ (meshPositionMap (Vec3.add (vec3 0 1.5 0))
      (meshPositionMap (Vec3.scale 0.1) (icosaMeshList axisColor))),
      (meshPositionMap (Vec3.add (vec3 0 -1.5 0))
      (meshPositionMap (Vec3.scale 0.1) (icosaMeshList axisColor))),
      (meshPositionMap (Vec3.add (vec3 0 1.25 0))
      (meshPositionMap (Vec3.scale 0.1) (icosaMeshList axisColor))),
      (meshPositionMap (Vec3.add (vec3 0 -1.25 0))
      (meshPositionMap (Vec3.scale 0.1) (icosaMeshList axisColor)))
    ]
    |> List.concat
    |> WebGL.triangles


-- Constructs a simple mesh for sun

sunMesh : Mesh Vertex
sunMesh = 
  let sunColor = Vec3.scale (1/255) (vec3 237 212 0) -- yellow
  in icosaMeshList sunColor
  |> subdivideProject sunColor
  |> subdivideProject sunColor
  |> WebGL.triangles


-- Constructs a simple mesh for hero

heroMesh : Mesh Vertex
heroMesh = 
--let balloonColor = Vec3.scale (1/255) (vec3 115 210 22) -- green
  let balloonColor = Vec3.scale (1/255) (vec3 237 212 0) -- yellow
  in 
    [ cubeMeshList
    , meshPositionMap 
       (Vec3.add (vec3 0 4 0)) 
       (meshPositionMap 
        (Vec3.scale 2) 
        (subdivideProject balloonColor (icosaMeshList balloonColor)))
    ]
    |> List.concat
    |> WebGL.triangles


-- Constructs a simple fire mesh,
-- is separate from hero as the scale
-- is adjustable

fireMesh : Mesh Vertex
fireMesh = 
  let fireColor = Vec3.scale (1/ 255) (vec3 245 121 0) -- orange
  in 
    [ 
      icosaMeshList fireColor
    ]
    |> List.concat
    |> WebGL.triangles


-- Helper to create a icosahedron vertex list, which serve as a good
-- approximation for spheres

icosaMeshList : Vec3 -> MeshList
icosaMeshList clr =
  let phi = (1.0 + sqrt 5.0) * 0.5
      a = 1.0
      b = 1.0 / phi

      v1 = Vertex (Vec3.add clr (vec3 0.0 0.0 0.0)) (Vec3.normalize (vec3 0 b -a))
      v2 = Vertex (Vec3.add clr (vec3 0.02 0.02 0.02)) (Vec3.normalize (vec3 b a 0))
      v3 = Vertex (Vec3.add clr (vec3 0.04 0.04 0.04)) (Vec3.normalize (vec3 -b a 0))
      v4 = Vertex (Vec3.add clr (vec3 0.06 0.06 0.06)) (Vec3.normalize (vec3 0 b a))
      v5 = Vertex (Vec3.add clr (vec3 0.08 0.08 0.08)) (Vec3.normalize (vec3 0 -b a))
      v6 = Vertex (Vec3.add clr (vec3 0.10 0.10 0.10)) (Vec3.normalize (vec3 -a 0 b))
      v7 = Vertex (Vec3.add clr (vec3 0.12 0.12 0.12)) (Vec3.normalize (vec3 0 -b -a))
      v8 = Vertex (Vec3.add clr (vec3 0.14 0.14 0.14)) (Vec3.normalize (vec3 a 0 -b))
      v9 = Vertex (Vec3.add clr (vec3 0.16 0.16 0.16)) (Vec3.normalize (vec3 a 0 b))
      v10 = Vertex (Vec3.add clr (vec3 0.18 0.18 0.18)) (Vec3.normalize (vec3 -a 0 -b))
      v11 = Vertex (Vec3.add clr (vec3 0.20 0.20 0.20)) (Vec3.normalize (vec3 b -a 0))
      v12 = Vertex (Vec3.add clr (vec3 0.22 0.22 0.22)) (Vec3.normalize (vec3 -b -a 0))

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


-- Helper to create a cube vertex list

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


-- Cube helper

face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> MeshList
face color a b c d =
  let
    vertex position =
      Vertex (Vec3.scale (1 / 255) color) position
  in
    [ ( vertex a, vertex b, vertex c )
    , ( vertex c, vertex d, vertex a )
    ]


-- Helper to subdivide icosahedrons to
-- make better spheres

subdivideProject : Vec3 -> MeshList -> MeshList
subdivideProject clr mesh =
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

                -- clr12R = ((Vec3.getX v1.color) +
                --           (Vec3.getX v2.color)) / 2
                -- clr12G = ((Vec3.getY v1.color) +
                --           (Vec3.getY v2.color)) / 2
                -- clr12B = ((Vec3.getZ v1.color) +
                --           (Vec3.getZ v2.color)) / 2

                -- clr13R = ((Vec3.getX v1.color) +
                --           (Vec3.getX v3.color)) / 2
                -- clr13G = ((Vec3.getY v1.color) +
                --           (Vec3.getY v3.color)) / 2
                -- clr13B = ((Vec3.getZ v1.color) +
                --           (Vec3.getZ v3.color)) / 2

                -- clr23R = ((Vec3.getX v2.color) +
                --           (Vec3.getX v3.color)) / 2
                -- clr23G = ((Vec3.getY v2.color) +
                --           (Vec3.getY v3.color)) / 2
                -- clr23B = ((Vec3.getZ v2.color) +
                --           (Vec3.getZ v3.color)) / 2

                -- clr12 = vec3 clr12R clr12G clr12B
                -- clr13 = vec3 clr13R clr13G clr13B
                -- clr23 = vec3 clr23R clr23G clr23B

                clr12 = clr
                clr13 = clr
                clr23 = clr

                pos12 = vec3 mp12X mp12Y mp12Z
                pos13 = vec3 mp13X mp13Y mp13Z
                pos23 = vec3 mp23X mp23Y mp23Z

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
 

-- Creates a simple camera that looks everything from
-- a distance

makeOverviewCamera : Model -> Mat4
makeOverviewCamera model =
    (Mat4.makeLookAt (vec3 5 0 5)
                     (vec3 model.earth.locationX
                           model.earth.locationY
                           model.earth.locationZ)
                     (vec3 0 1 0))


-- Creates a hero camera, following hero.

makeHeroCamera : Model -> Mat4
makeHeroCamera model =
  let azimoth = model.camera.azimoth
      elevation = model.camera.elevation

      earthLoc = (vec3 model.earth.locationX
                       model.earth.locationY
                       model.earth.locationZ)

      -- Generate a general rotation matrix that can transform
      -- the hero, the camera and even the up vector.
      earthAxis = (Mat4.transform 
                   (Mat4.makeRotate ((23.5/180)*pi) (vec3 0 0 1))
                   Vec3.j)
      latitudeAxis = Vec3.cross earthAxis (vec3 1 0 0)
      alignRotation = (Mat4.makeRotate ((23.5/180)*pi) (vec3 0 0 1))
      latitudeRotation = (Mat4.makeRotate (pi/2 - model.hero.latitude) latitudeAxis)
      longitudeRotation = (Mat4.makeRotate (model.hero.longitude) earthAxis)
      earthRotationRotation = (Mat4.makeRotate model.earth.rotationTheta earthAxis)
      rotateAround = (List.foldl
                      Mat4.mul 
                      Mat4.identity [alignRotation,
                                     latitudeRotation,
                                     longitudeRotation,
                                     earthRotationRotation])

      -- Find out the target location
      targetTransformation = 
        (List.foldl 
         Mat4.mul 
         Mat4.identity 
         [
          (Mat4.translate (vec3 0 model.hero.height 0) Mat4.identity),
          rotateAround,
          (Mat4.translate earthLoc Mat4.identity)
         ])
      targetLocation = Mat4.transform targetTransformation (vec3 0 0 0)

      -- Find out the camera location.
      -- Sits behind the hero.
      -- Also apply the user controlled parameters azimoth and elevation.
      locStart = vec3 0 0 0.05
      locAz = Mat4.transform (Mat4.makeRotate azimoth (vec3 0 1 0)) locStart
      locElv = Mat4.transform (Mat4.makeRotate elevation 
                               (Vec3.cross locAz (vec3 0 1 0))) locAz
      cameraTransformation = 
        (List.foldl 
         Mat4.mul 
         Mat4.identity 
         [
          (Mat4.translate (vec3 0 model.hero.height 0) Mat4.identity),
          rotateAround,
          (Mat4.translate earthLoc Mat4.identity)
         ])
      cameraLocation = Mat4.transform cameraTransformation locElv

      -- And finally the up direction.
      up = Mat4.transform rotateAround Vec3.j
  in
    Mat4.makeLookAt cameraLocation
                    targetLocation
                    up


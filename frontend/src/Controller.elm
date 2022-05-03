module Controller exposing
    ( controllerMeshDown
    , controllerMeshUp
    , controllerUnif
    , coordinatesWithinDownButton
    , coordinatesWithinUpButton
    )

import Common
    exposing
        ( GameData
        , MeshList
        , Uniforms
        , Vertex
        , meshPositionMap
        , viewportSize
        )
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh)



-- Some global controller related variables


controllerParams : { x : Float, y : Float, size : Float, trans : Float }
controllerParams =
    { x = 0.5
    , y = -0.2
    , size = 0.2
    , trans = 0.3
    }



-- Controller uniforms


controllerUnif : GameData -> Float -> Uniforms
controllerUnif gameData shade =
  case gameData.canvasDimensions of
    Just canvasDimensions ->
      let
          xscale =
              toFloat canvasDimensions.width
                  / toFloat (Tuple.first viewportSize)
  
          yscale =
              toFloat canvasDimensions.height
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
    Nothing ->
      { preScale = Mat4.identity
      , preRotation = Mat4.identity
      , preTranslation = Mat4.identity
      , scale = Mat4.identity
      , rotation = Mat4.identity
      , translation = Mat4.identity
      , postScale = Mat4.identity
      , postRotation = Mat4.identity
      , postTranslation = Mat4.identity
      , perspective = 
          Mat4.makeOrtho -1 1 -1 1 0 10
      , camera =
          Mat4.makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
      , shade = shade
      }

  
  

-- Controller meshes


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



-- Helpers for deciding if touch / cursor location
-- is within a control


coordinatesWithinUpButton : GameData -> ( Float, Float ) -> Bool
coordinatesWithinUpButton gameData offset =
    coordinatesWithinButton gameData offset (controllerParams.trans + 0.5)


coordinatesWithinDownButton : GameData -> ( Float, Float ) -> Bool
coordinatesWithinDownButton gameData offset =
    coordinatesWithinButton gameData offset (-controllerParams.trans - 0.5)


coordinatesWithinButton : GameData -> ( Float, Float ) -> Float -> Bool
coordinatesWithinButton gameData pointerOffset trans =
  case gameData.canvasDimensions of
    Just canvasDimensions ->
      let
          yscale =
              toFloat canvasDimensions.height
                  / toFloat (Tuple.second viewportSize)

          xscale =
              toFloat canvasDimensions.width
                  / toFloat (Tuple.first viewportSize)

          size =
              controllerParams.size

          fixedTrans =
              (trans * size) / yscale

          middlepointX =
              (1 + controllerParams.x) * (toFloat canvasDimensions.width / 2)

          middlepointY =
              (1 - controllerParams.y - fixedTrans) * (toFloat canvasDimensions.height / 2)

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

    Nothing -> False
  

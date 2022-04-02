module Main exposing (main)


import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (normalize, vec3, Vec3)
import WebGL exposing (Mesh, Shader)


type alias Model = 
  { location : { x : Float, y: Float, z: Float }
  , rotation : Float
  , elapsed : Float
  }


type alias Msg = 
  Float


type alias Uniforms = 
  { rotation : Mat4
  , location : Mat4
  , perspective : Mat4
  , camera : Mat4
  , shade : Float
  }


type alias Vertex =
  { color : Vec3
  , position : Vec3 
  }

type alias MeshList = List (Vertex, Vertex, Vertex)


init : () -> (Model, Cmd Msg)
init _ = 
  (Model {x = 0, y = 0, z = 0} 0 0, Cmd.none)


view : Model -> Html Msg
view model =
  div [] 
    [
      div [] [text (String.fromFloat model.elapsed)],
      WebGL.toHtml
        [ width 600
        , height 600
        , style "display" "block"
        , style "width" "100%"
        ]
        [ (WebGL.entity
          vertexShader
          fragmentShader
          heroMesh
          (uniforms model))
        ]
    ]


subscriptions : Model -> Sub Msg
subscriptions _ = 
  onAnimationFrameDelta Basics.identity


update : Msg -> Model -> (Model, Cmd Msg)
update dt model = 
  ( let locationRec = model.location
    in
    { model | rotation = sin (model.elapsed / 1000) / 10, 
              location = { locationRec | x = 0.5 * sin (model.elapsed / 1000) },
              elapsed = model.elapsed + dt
    } 
  , Cmd.none 
  )


main : Program () Model Msg
main =
  Browser.element { init = init
                  , view = view
                  , subscriptions = subscriptions
                  , update = update }



uniforms : Model -> Uniforms
uniforms model =
  { rotation = 
      Mat4.mul (Mat4.makeRotate (3 * model.rotation) (vec3 0 1 0))
               (Mat4.makeRotate (2 * model.rotation) (vec3 1 0 0))
  , location = 
      Mat4.translate (vec3 model.location.x 0 0) Mat4.identity

  , perspective = 
      Mat4.makePerspective 45 1 0.01 100

  , camera = 
      Mat4.makeLookAt (vec3 0 0 15) (vec3 0 0 0) (vec3 0 1 0)

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
  [ cubeMeshList
  , meshPositionMap 
     (Vec3.add (vec3 0 4 0)) 
     (meshPositionMap (Vec3.scale 2) balloonMeshList)
  ]
  |> List.concat
  |> WebGL.triangles


balloonMeshList : MeshList
balloonMeshList =
  let phi = (1.0 + sqrt 5.0) * 0.5
      a = 1.0
      b = 1.0 / phi
      clr = Vec3.scale (1/ 255) (vec3 115 210 22) -- green
      v1 = Vertex clr (normalize (vec3 0 b -a))
      v2 = Vertex clr (normalize (vec3 b a 0))
      v3 = Vertex clr (normalize (vec3 -b a 0))
      v4 = Vertex clr (normalize (vec3 0 b a))
      v5 = Vertex clr (normalize (vec3 0 -b a))
      v6 = Vertex clr (normalize (vec3 -a 0 b))
      v7 = Vertex clr (normalize (vec3 0 -b a))
      v8 = Vertex clr (normalize (vec3 a 0 -b))
      v9 = Vertex clr (normalize (vec3 a 0 b))
      v10 = Vertex clr (normalize (vec3 -a 0 -b))
      v11 = Vertex clr (normalize (vec3 b -a 0))
      v12 = Vertex clr (normalize (vec3 -b -a 0))
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
     varying vec3 vcolor;
     void main () {
       gl_Position = (perspective * camera * location * 
                      rotation * vec4(position, 1.0));
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

